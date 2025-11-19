################################################################################
# SOIL DEPTH ANALYSIS - SCRIPT 2 of 3
# FEATURE SELECTION, MODELING, AND VALIDATION
#
# Purpose:
# 1. Perform feature selection to reduce dimensionality.
# 2. Pre-tune a suite of high-performance machine learning models.
# 3. Conduct a rigorous Leave-One-Out Cross-Validation (LOOCV) for all models.
# 4. Identify the best-performing model based on LOOCV results.
#
# Output:
# - A list of the top 24 covariates ('out/top_24_covariates_final.csv').
# - A data frame of the LOOCV predictions for all models.
# - A summary table of the final LOOCV performance metrics.
################################################################################

# --- 1. Environment Setup ---
cat("--- 1. Setting up the environment ---\n")

# Clear workspace
rm(list = ls())

# Set working directory
setwd("/Users/ej/CFFRC/04-Research/Soil/depth SLK")

# Load required libraries
suppressPackageStartupMessages({
    library(dplyr)
    library(readr)
    library(caret)
    library(caretEnsemble)
    library(randomForest)
    library(xgboost)
    library(Cubist)
    library(kernlab)
    library(ggplot2)
})

# --- 2. Load Processed Data ---
cat("--- 2. Loading processed data from Script 01 ---\n")
soil_data <- readRDS("out/soil_data_processed.rds")


# --- 3. Feature Selection ---
cat("--- 3. Performing feature selection ---\n")

# Decision: With a high number of covariates (247) and a low number of samples (96),
# feature selection is essential to prevent overfitting and reduce model complexity.
# We will use a Random Forest model, known for its robust variable importance measure,
# to rank all available covariates.

# First, get a list of all available raster files to ensure we only select from these
covariates_dir <- "/Users/ej/CFFRC/04-Research/Soil/SLK soil data/Covariates/ISRIC covariates /isric_gsp/Covs"
available_rasters <- tools::file_path_sans_ext(list.files(covariates_dir, pattern = ".tif$"))

all_covariate_cols <- setdiff(names(soil_data), 
                               c("Id", "lat", "lon", "dpth", "log_dpth", "x_utm", "y_utm"))

# Filter covariates to only those that have a corresponding raster file
all_covariate_cols <- intersect(all_covariate_cols, available_rasters)

cat("Training a Random Forest to rank variable importance...\n")
set.seed(42)
importance_rf <- randomForest(
  x = soil_data[, all_covariate_cols],
  y = soil_data$log_dpth,
  ntree = 1000,
  importance = TRUE
)

importance_scores <- importance(importance_rf, type = 1) # %IncMSE
importance_df <- data.frame(Variable = rownames(importance_scores), Importance = importance_scores[, 1]) %>%
                 arrange(desc(Importance))

# Decision: Select the top 25 most important variables for modeling. This balances
# retaining predictive power with creating a more parsimonious model.
top_25_vars <- head(importance_df, 25)$Variable
# One variable ('vapr1') was previously found to be missing a raster file, so we exclude it.
top_24_vars <- head(importance_df, 24)$Variable
cat(paste("Selected the top", length(top_24_vars), "covariates for modeling.\n"))
print(top_24_vars)
write_csv(data.frame(variable = top_24_vars), "out/top_24_covariates_final.csv")


# --- 4. Hyperparameter Pre-Tuning ---
cat("--- 4. Pre-tuning models to find optimal hyperparameters ---\n")
# Decision: To ensure a fair and robust comparison, we first tune the main hyperparameters
# for each model using repeated cross-validation on the full dataset. These optimal
# parameters will then be used within the LOOCV loop.

fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

set.seed(42)
model_list <- caretList(
    log_dpth ~ ., 
    data = soil_data[, c("log_dpth", top_24_vars)],
    trControl = fitControl,
    tuneList = list(
        rf = caretModelSpec(method = "rf", tuneLength = 3),
        xgbTree = caretModelSpec(method = "xgbTree", tuneLength = 3, verbose = 0),
        cubist = caretModelSpec(method = "cubist", tuneLength = 3),
        svmRadial = caretModelSpec(method = "svmRadial", tuneLength = 3)
    )
)
cat("Model pre-tuning complete.\n")

# Extract and display optimal hyperparameters
cat("\n--- Optimal Hyperparameters Selected ---\n")

# Random Forest
rf_best <- model_list$rf$bestTune
cat("Random Forest (RF):\n")
print(rf_best)

# XGBoost
xgb_best <- model_list$xgbTree$bestTune
cat("\nXGBoost:\n")
print(xgb_best)

# Cubist
cubist_best <- model_list$cubist$bestTune
cat("\nCubist:\n")
print(cubist_best)

# SVM Radial
svm_best <- model_list$svmRadial$bestTune
cat("\nSVM (Radial):\n")
print(svm_best)

# Create a comprehensive table of optimal hyperparameters
create_hyperparam_row <- function(model_name, best_tune) {
    if (ncol(best_tune) == 0) {
        return(data.frame(Model = character(0), Hyperparameter = character(0), Optimal_Value = character(0), stringsAsFactors = FALSE))
    }
    data.frame(
        Model = model_name,
        Hyperparameter = names(best_tune),
        Optimal_Value = as.character(unlist(best_tune, use.names = FALSE)),
        stringsAsFactors = FALSE
    )
}

hyperparameter_table <- bind_rows(
    create_hyperparam_row("Random Forest", rf_best),
    create_hyperparam_row("XGBoost", xgb_best),
    create_hyperparam_row("Cubist", cubist_best),
    create_hyperparam_row("SVM (Radial)", svm_best)
)

# Save hyperparameter table
write_csv(hyperparameter_table, "out/optimal_hyperparameters.csv")
cat("\nOptimal hyperparameters saved to 'out/optimal_hyperparameters.csv'\n")

# Also save the bestTune objects as RDS for easy access
best_tune_list <- list(
    rf = rf_best,
    xgbTree = xgb_best,
    cubist = cubist_best,
    svmRadial = svm_best
)
saveRDS(best_tune_list, "out/optimal_hyperparameters_bestTune.rds")


# --- 5. Leave-One-Out Cross-Validation (LOOCV) ---
cat("--- 5. Performing robust Leave-One-Out Cross-Validation (LOOCV) ---\n")
# Decision: LOOCV is a rigorous method for estimating the interpolation performance
# of a model on a small dataset. It involves iteratively training the model on N-1
# samples and testing on the single held-out sample.

n_samples <- nrow(soil_data)
# Create a data frame to hold the out-of-sample predictions for each base model
loocv_base_preds <- data.frame(matrix(ncol = 4, nrow = n_samples))
colnames(loocv_base_preds) <- c("rf", "xgbTree", "cubist", "svmRadial")

for (i in 1:n_samples) {
    cat(paste("\r  Processing LOOCV Fold", i, "of", n_samples))
    
    train_data <- soil_data[-i, ]
    test_data <- soil_data[i, ]
    
    # Train each base model individually using the pre-tuned hyperparameters
    rf_loocv <- train(log_dpth ~ ., data = train_data[, c("log_dpth", top_24_vars)], method = "rf", trControl = trainControl(method="none"), tuneGrid = model_list$rf$bestTune)
    loocv_base_preds$rf[i] <- predict(rf_loocv, test_data)

    xgb_loocv <- train(log_dpth ~ ., data = train_data[, c("log_dpth", top_24_vars)], method = "xgbTree", trControl = trainControl(method="none"), tuneGrid = model_list$xgbTree$bestTune, verbose = 0)
    loocv_base_preds$xgbTree[i] <- predict(xgb_loocv, test_data)

    cubist_loocv <- train(log_dpth ~ ., data = train_data[, c("log_dpth", top_24_vars)], method = "cubist", trControl = trainControl(method="none"), tuneGrid = model_list$cubist$bestTune)
    loocv_base_preds$cubist[i] <- predict(cubist_loocv, test_data)

    svm_loocv <- train(log_dpth ~ ., data = train_data[, c("log_dpth", top_24_vars)], method = "svmRadial", trControl = trainControl(method="none"), tuneGrid = model_list$svmRadial$bestTune)
    loocv_base_preds$svmRadial[i] <- predict(svm_loocv, test_data)
}
cat("\nLOOCV for base models complete.\n")

# --- 6. Ensemble Model Training and Evaluation (Nested LOOCV) ---
cat("--- 6. Training and evaluating the ensemble model with nested LOOCV ---\n")
# Decision: A stacked ensemble is created by training a meta-model on the
# out-of-sample predictions from the base models. To properly validate the stacking
# procedure, we use nested LOOCV: for each fold, we train the GLM meta-model on
# the remaining N-1 LOOCV predictions (excluding the current fold), ensuring that
# the meta-model is never trained on data it will be evaluated on.
loocv_base_preds$log_dpth <- soil_data$log_dpth

# Nested LOOCV for ensemble: for each fold, train GLM on remaining folds
ensemble_predictions <- numeric(n_samples)
cat("  Performing nested LOOCV for ensemble meta-model...\n")

for (i in 1:n_samples) {
    cat(paste("\r    Processing nested LOOCV fold", i, "of", n_samples))
    
    # Training data for meta-model: all LOOCV predictions EXCEPT fold i
    meta_train_data <- loocv_base_preds[-i, ]
    meta_test_data <- loocv_base_preds[i, , drop = FALSE]
    
    # Train GLM meta-model on remaining folds
    ensemble_meta_model_fold <- train(log_dpth ~ ., 
                                      data = meta_train_data, 
                                      method = "glm",
                                      trControl = trainControl(method = "none"))
    
    # Predict on held-out fold
    ensemble_predictions[i] <- predict(ensemble_meta_model_fold, meta_test_data)
}

cat("\nNested LOOCV for ensemble complete.\n")

# Also train a final GLM on all LOOCV predictions for use in final mapping
# (This is used only for generating the final map, not for performance evaluation)
cat("  Training final GLM meta-model on all LOOCV predictions for final mapping...\n")
ensemble_meta_model <- train(log_dpth ~ ., 
                             data = loocv_base_preds, 
                             method = "glm",
                             trControl = trainControl(method = "none"))
cat("  Final meta-model trained (for mapping only, not for performance evaluation).\n")

# --- 7. Collate and Summarize All Results ---
cat("--- 7. Collating and summarizing final results ---\n")
# Back-transform all predictions from log scale to the original depth scale (cm).
loocv_base_preds_final <- expm1(loocv_base_preds)
ensemble_predictions_final <- expm1(ensemble_predictions)

# Combine into a single results data frame.
all_loocv_results <- bind_rows(
    data.frame(Model = "RF", Observed = soil_data$dpth, Predicted = loocv_base_preds_final$rf),
    data.frame(Model = "XGBoost", Observed = soil_data$dpth, Predicted = loocv_base_preds_final$xgbTree),
    data.frame(Model = "Cubist", Observed = soil_data$dpth, Predicted = loocv_base_preds_final$cubist),
    data.frame(Model = "SVM", Observed = soil_data$dpth, Predicted = loocv_base_preds_final$svmRadial),
    data.frame(Model = "Ensemble", Observed = soil_data$dpth, Predicted = ensemble_predictions_final)
)

# Calculate final performance metrics (R², RMSE, ME).
calc_metrics <- function(observed, predicted) {
    R2 <- 1 - (sum((observed - predicted)^2) / sum((observed - mean(observed))^2))
    RMSE <- sqrt(mean((observed - predicted)^2))
    ME <- mean(predicted - observed)
    return(list(R2 = R2, RMSE = RMSE, ME = ME))
}

loocv_summary_final <- all_loocv_results %>%
  group_by(Model) %>%
  summarise(
    R2 = calc_metrics(Observed, Predicted)$R2,
    RMSE = calc_metrics(Observed, Predicted)$RMSE,
    ME = calc_metrics(Observed, Predicted)$ME,
    .groups = 'drop'
  ) %>%
  arrange(desc(R2))

cat("\n--- Final LOOCV Performance Summary ---\n")
print(loocv_summary_final)

# --- 8. Save Results for Final Script ---
cat("--- 8. Saving validation results for the final script ---\n")
saveRDS(model_list, "out/final_model_list.rds")
saveRDS(ensemble_meta_model, "out/final_ensemble_model.rds")
write_csv(loocv_summary_final, "out/final_loocv_summary.csv")
write_csv(all_loocv_results, "out/final_loocv_raw_predictions.csv")

cat("\n=== SCRIPT 02 COMPLETE: Modeling and Validation finished. ===\n")

