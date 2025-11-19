################################################################################
# SOIL DEPTH ANALYSIS - SCRIPT 6
# SPATIAL CROSS-VALIDATION AND UNCERTAINTY QUANTIFICATION
#
# Purpose:
# 1. Perform spatial cross-validation using buffered leave-location-out approach
# 2. Compare spatial CV results with LOOCV to assess spatial autocorrelation effects
# 3. Quantify prediction uncertainty (standard deviation and prediction intervals)
# 4. Generate uncertainty maps
#
# Output:
# - Spatial CV performance metrics ('out/spatial_cv_summary.csv')
# - Comparison table of LOOCV vs Spatial CV ('out/cv_comparison.csv')
# - Uncertainty maps ('out/uncertainty_map_sd.tif', 'out/uncertainty_map_pi.tif')
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
    library(sf)
    library(terra)
    library(caret)
    library(randomForest)
    library(xgboost)
    library(Cubist)
    library(kernlab)
    library(ggplot2)
    library(viridis)
})

# --- 2. Load Data and Models ---
cat("--- 2. Loading data and pre-trained models ---\n")
soil_data <- readRDS("out/soil_data_processed.rds")
model_list <- readRDS("out/final_model_list.rds")
ensemble_meta_model <- readRDS("out/final_ensemble_model.rds")
top_24_vars <- read_csv("out/top_24_covariates_final.csv", show_col_types = FALSE)$variable

# Create spatial object for distance calculations
soil_sf <- st_as_sf(soil_data, coords = c("x_utm", "y_utm"), crs = 32644, remove = FALSE)

# --- 3. Spatial Cross-Validation: Buffered Leave-Location-Out ---
cat("--- 3. Performing spatial cross-validation (buffered leave-location-out) ---\n")

# Decision: For spatial data, we need to account for spatial autocorrelation.
# Buffered leave-location-out excludes all points within a buffer distance of the test point.
# This provides a more realistic estimate of out-of-area prediction skill.

# Calculate pairwise distances between all points (in meters)
dist_matrix <- st_distance(soil_sf, soil_sf)
dist_matrix <- as.matrix(dist_matrix)
# Convert units to numeric (meters)
if (inherits(dist_matrix, "units")) {
    dist_matrix <- units::drop_units(dist_matrix)
}

# Choose buffer distance: 5 km (5000 m) to exclude nearby points
# This distance is chosen to be larger than typical spatial autocorrelation range
# but small enough to retain sufficient training data
buffer_distance <- 5000  # 5 km in meters

cat(paste("Using buffer distance of", buffer_distance/1000, "km\n"))

n_samples <- nrow(soil_data)
spatial_cv_base_preds <- data.frame(matrix(ncol = 4, nrow = n_samples))
colnames(spatial_cv_base_preds) <- c("rf", "xgbTree", "cubist", "svmRadial")

# Track how many points are excluded in each fold
n_excluded <- numeric(n_samples)

for (i in 1:n_samples) {
    cat(paste("\r  Processing Spatial CV Fold", i, "of", n_samples))
    
    # Find points within buffer distance of test point
    nearby_points <- which(dist_matrix[i, ] < buffer_distance)
    # Exclude the test point itself
    nearby_points <- setdiff(nearby_points, i)
    
    n_excluded[i] <- length(nearby_points)
    
    # Create training set: exclude test point and all nearby points
    train_indices <- setdiff(1:n_samples, c(i, nearby_points))
    
    if (length(train_indices) < 10) {
        cat(paste("\n  Warning: Fold", i, "has only", length(train_indices), "training samples. Skipping.\n"))
        next
    }
    
    train_data <- soil_data[train_indices, ]
    test_data <- soil_data[i, ]
    
    # Train each base model using pre-tuned hyperparameters
    tryCatch({
        rf_spatial <- train(log_dpth ~ ., 
                           data = train_data[, c("log_dpth", top_24_vars)], 
                           method = "rf", 
                           trControl = trainControl(method="none"), 
                           tuneGrid = model_list$rf$bestTune)
        spatial_cv_base_preds$rf[i] <- predict(rf_spatial, test_data)
    }, error = function(e) {
        cat(paste("\n  Error in RF fold", i, ":", e$message, "\n"))
    })
    
    tryCatch({
        xgb_spatial <- train(log_dpth ~ ., 
                            data = train_data[, c("log_dpth", top_24_vars)], 
                            method = "xgbTree", 
                            trControl = trainControl(method="none"), 
                            tuneGrid = model_list$xgbTree$bestTune, 
                            verbose = 0)
        spatial_cv_base_preds$xgbTree[i] <- predict(xgb_spatial, test_data)
    }, error = function(e) {
        cat(paste("\n  Error in XGBoost fold", i, ":", e$message, "\n"))
    })
    
    tryCatch({
        cubist_spatial <- train(log_dpth ~ ., 
                               data = train_data[, c("log_dpth", top_24_vars)], 
                               method = "cubist", 
                               trControl = trainControl(method="none"), 
                               tuneGrid = model_list$cubist$bestTune)
        spatial_cv_base_preds$cubist[i] <- predict(cubist_spatial, test_data)
    }, error = function(e) {
        cat(paste("\n  Error in Cubist fold", i, ":", e$message, "\n"))
    })
    
    tryCatch({
        svm_spatial <- train(log_dpth ~ ., 
                            data = train_data[, c("log_dpth", top_24_vars)], 
                            method = "svmRadial", 
                            trControl = trainControl(method="none"), 
                            tuneGrid = model_list$svmRadial$bestTune)
        spatial_cv_base_preds$svmRadial[i] <- predict(svm_spatial, test_data)
    }, error = function(e) {
        cat(paste("\n  Error in SVM fold", i, ":", e$message, "\n"))
    })
}
cat("\nSpatial CV for base models complete.\n")

# Calculate ensemble predictions
spatial_cv_base_preds$log_dpth <- soil_data$log_dpth
# Remove rows with NA predictions
valid_rows <- complete.cases(spatial_cv_base_preds[, 1:4])
spatial_cv_base_preds_valid <- spatial_cv_base_preds[valid_rows, ]

if (nrow(spatial_cv_base_preds_valid) > 0) {
    ensemble_spatial_preds <- predict(ensemble_meta_model, 
                                     spatial_cv_base_preds_valid[, 1:4])
    spatial_cv_base_preds_valid$ensemble <- ensemble_spatial_preds
} else {
    cat("Warning: No valid spatial CV predictions for ensemble model.\n")
    spatial_cv_base_preds_valid$ensemble <- NA
}

# Back-transform predictions
spatial_cv_results <- data.frame(
    Model = rep(c("RF", "XGBoost", "Cubist", "SVM", "Ensemble"), 
                each = nrow(spatial_cv_base_preds_valid)),
    Observed = rep(expm1(spatial_cv_base_preds_valid$log_dpth), 5),
    Predicted = c(expm1(spatial_cv_base_preds_valid$rf),
                  expm1(spatial_cv_base_preds_valid$xgbTree),
                  expm1(spatial_cv_base_preds_valid$cubist),
                  expm1(spatial_cv_base_preds_valid$svmRadial),
                  expm1(spatial_cv_base_preds_valid$ensemble))
) %>%
    filter(!is.na(Predicted))

# Calculate performance metrics
calc_metrics <- function(observed, predicted) {
    R2 <- 1 - (sum((observed - predicted)^2) / sum((observed - mean(observed))^2))
    RMSE <- sqrt(mean((observed - predicted)^2))
    ME <- mean(predicted - observed)
    return(list(R2 = R2, RMSE = RMSE, ME = ME))
}

spatial_cv_summary <- spatial_cv_results %>%
    group_by(Model) %>%
    summarise(
        n = n(),
        R2 = calc_metrics(Observed, Predicted)$R2,
        RMSE = calc_metrics(Observed, Predicted)$RMSE,
        ME = calc_metrics(Observed, Predicted)$ME,
        .groups = 'drop'
    ) %>%
    arrange(desc(R2))

cat("\n--- Spatial CV Performance Summary ---\n")
print(spatial_cv_summary)

# Save spatial CV results
write_csv(spatial_cv_summary, "out/spatial_cv_summary.csv")
write_csv(spatial_cv_results, "out/spatial_cv_raw_predictions.csv")

# --- 4. Compare LOOCV vs Spatial CV ---
cat("--- 4. Comparing LOOCV and Spatial CV results ---\n")
loocv_summary <- read_csv("out/final_loocv_summary.csv", show_col_types = FALSE)

cv_comparison <- bind_rows(
    loocv_summary %>% mutate(CV_Method = "LOOCV"),
    spatial_cv_summary %>% mutate(CV_Method = "Spatial CV (5 km buffer)")
) %>%
    select(CV_Method, Model, n, R2, RMSE, ME) %>%
    arrange(Model, CV_Method)

cat("\n--- Comparison: LOOCV vs Spatial CV ---\n")
print(cv_comparison)
write_csv(cv_comparison, "out/cv_comparison.csv")

# --- 5. Uncertainty Quantification ---
cat("--- 5. Quantifying prediction uncertainty ---\n")

# Load final map for uncertainty estimation
final_map <- rast("out/Final_Ensemble_Soil_Depth_Map.tif")

# For uncertainty, we'll use the standard deviation of predictions from multiple models
# and calculate prediction intervals based on the spatial CV residuals

# Calculate residuals from spatial CV for ensemble model
ensemble_spatial_residuals <- spatial_cv_results %>%
    filter(Model == "Ensemble") %>%
    mutate(Residual = Observed - Predicted)

# Calculate residual standard deviation
residual_sd <- sd(ensemble_spatial_residuals$Residual, na.rm = TRUE)
cat(paste("Residual standard deviation from spatial CV:", round(residual_sd, 2), "cm\n"))

# Create uncertainty map: prediction standard deviation
# For simplicity, we'll use a constant uncertainty based on spatial CV residuals
# In a more sophisticated approach, this could vary spatially based on distance to samples
uncertainty_sd <- final_map
values(uncertainty_sd) <- residual_sd
names(uncertainty_sd) <- "Prediction_SD"

# Create prediction interval map (95% confidence interval: ±1.96 * SD)
uncertainty_pi_lower <- final_map - 1.96 * residual_sd
uncertainty_pi_upper <- final_map + 1.96 * residual_sd

# Save uncertainty maps
writeRaster(uncertainty_sd, "out/uncertainty_map_sd.tif", overwrite = TRUE)
writeRaster(uncertainty_pi_lower, "out/uncertainty_map_pi_lower.tif", overwrite = TRUE)
writeRaster(uncertainty_pi_upper, "out/uncertainty_map_pi_upper.tif", overwrite = TRUE)

cat("Uncertainty maps saved.\n")

# --- 6. Create Visualization ---
cat("--- 6. Creating comparison visualization ---\n")

# Plot comparing LOOCV vs Spatial CV
comparison_plot <- cv_comparison %>%
    ggplot(aes(x = Model, y = R2, fill = CV_Method)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Model Performance: LOOCV vs Spatial Cross-Validation",
         subtitle = "Spatial CV uses 5 km buffer to account for spatial autocorrelation",
         x = "Model", y = "R²", fill = "Validation Method") +
    theme_minimal() +
    theme(legend.position = "bottom")

ggsave("out/Figure_CV_Comparison.png", comparison_plot, width = 10, height = 6, dpi = 300, bg = "white")

cat("\n=== SCRIPT 06 COMPLETE: Spatial Cross-Validation and Uncertainty Quantification finished. ===\n")

