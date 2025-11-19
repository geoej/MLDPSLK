################################################################################
# SOIL DEPTH ANALYSIS - SCRIPT 11
# FEATURE SET SIZE ANALYSIS
#
# Purpose:
# 1. Evaluate model performance with different numbers of selected features
# 2. Test feature set sizes: 10, 15, 20, 24, 30, 40
# 3. Generate performance curves to justify the selection of 24 features
# 4. Create visualization showing performance vs. number of features
#
# Output:
# - Performance comparison table for different feature set sizes
# - Performance curve plot (elbow plot)
# - Summary statistics
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
    library(randomForest)
    library(ggplot2)
    library(viridis)
})

# --- 2. Load Data ---
cat("--- 2. Loading data ---\n")
soil_data <- readRDS("out/soil_data_processed.rds")

# Get all available covariates
covariates_dir <- "/Users/ej/CFFRC/04-Research/Soil/SLK soil data/Covariates/ISRIC covariates /isric_gsp/Covs"
available_rasters <- tools::file_path_sans_ext(list.files(covariates_dir, pattern = ".tif$"))
all_covariate_cols <- setdiff(names(soil_data), 
                               c("Id", "lat", "lon", "dpth", "log_dpth", "x_utm", "y_utm"))
all_covariate_cols <- intersect(all_covariate_cols, available_rasters)

# --- 3. Get Variable Importance Ranking ---
cat("--- 3. Getting variable importance ranking ---\n")
cat("Training Random Forest to rank all variables...\n")
set.seed(42)
importance_rf <- randomForest(
  x = soil_data[, all_covariate_cols],
  y = soil_data$log_dpth,
  ntree = 1000,
  importance = TRUE
)

importance_scores <- importance(importance_rf, type = 1) # %IncMSE
importance_df <- data.frame(Variable = rownames(importance_scores), 
                           Importance = importance_scores[, 1]) %>%
                 arrange(desc(Importance))

# --- 4. Test Different Feature Set Sizes ---
cat("--- 4. Testing performance with different feature set sizes ---\n")

feature_sizes <- c(10, 15, 20, 24, 30, 40)
n_samples <- nrow(soil_data)

# Create results data frame
results_list <- list()

for (n_features in feature_sizes) {
    cat(paste("\nTesting with", n_features, "features...\n"))
    
    # Select top n features
    top_n_vars <- head(importance_df, n_features)$Variable
    
    # Perform LOOCV with Random Forest
    loocv_preds <- numeric(n_samples)
    
    for (i in 1:n_samples) {
        if (i %% 10 == 0) cat(paste("\r  LOOCV fold", i, "of", n_samples))
        
        train_data <- soil_data[-i, ]
        test_data <- soil_data[i, ]
        
        # Train RF model
        rf_model <- randomForest(
            x = train_data[, top_n_vars, drop = FALSE],
            y = train_data$log_dpth,
            ntree = 500,
            mtry = min(floor(sqrt(n_features)), n_features)
        )
        
        # Predict
        loocv_preds[i] <- predict(rf_model, test_data[, top_n_vars, drop = FALSE])
    }
    cat("\n")
    
    # Back-transform predictions
    loocv_preds_back <- expm1(loocv_preds)
    observed <- soil_data$dpth
    
    # Calculate metrics
    ss_res <- sum((observed - loocv_preds_back)^2)
    ss_tot <- sum((observed - mean(observed))^2)
    r2 <- 1 - (ss_res / ss_tot)
    rmse <- sqrt(mean((observed - loocv_preds_back)^2))
    me <- mean(loocv_preds_back - observed)
    
    results_list[[length(results_list) + 1]] <- data.frame(
        n_features = n_features,
        R2 = r2,
        RMSE = rmse,
        ME = me,
        n_samples = n_samples,
        ratio = n_samples / n_features
    )
}

# Combine results
feature_size_results <- bind_rows(results_list)

# --- 5. Save Results ---
cat("--- 5. Saving results ---\n")
write_csv(feature_size_results, "out/feature_set_size_analysis.csv")
cat("Results saved to 'out/feature_set_size_analysis.csv'\n")

# Print summary
cat("\n=== FEATURE SET SIZE ANALYSIS RESULTS ===\n")
print(feature_size_results)

# --- 6. Create Visualization ---
cat("--- 6. Creating performance curve visualization ---\n")

# R² plot
r2_plot <- ggplot(feature_size_results, aes(x = n_features, y = R2)) +
    geom_line(color = "blue", linewidth = 1.5) +
    geom_point(color = "blue", size = 3) +
    geom_vline(xintercept = 24, linetype = "dashed", color = "red", linewidth = 1) +
    annotate("text", x = 24, y = max(feature_size_results$R2) - 0.05, 
             label = "Selected: 24", color = "red", fontface = "bold") +
    labs(title = "Model Performance vs. Number of Features",
         subtitle = "Leave-One-Out Cross-Validation Results (Random Forest)",
         x = "Number of Features",
         y = "R²",
         caption = "Vertical line indicates selected feature set size (24)") +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold", size = 14),
          plot.subtitle = element_text(size = 11))

ggsave("out/Figure_Feature_Set_Size_Performance.png", r2_plot, 
       width = 8, height = 6, dpi = 300, bg = "white")
cat("Performance curve saved to 'out/Figure_Feature_Set_Size_Performance.png'\n")

# RMSE plot
rmse_plot <- ggplot(feature_size_results, aes(x = n_features, y = RMSE)) +
    geom_line(color = "darkgreen", linewidth = 1.5) +
    geom_point(color = "darkgreen", size = 3) +
    geom_vline(xintercept = 24, linetype = "dashed", color = "red", linewidth = 1) +
    annotate("text", x = 24, y = max(feature_size_results$RMSE) - 2, 
             label = "Selected: 24", color = "red", fontface = "bold") +
    labs(title = "Model Error vs. Number of Features",
         subtitle = "Leave-One-Out Cross-Validation Results (Random Forest)",
         x = "Number of Features",
         y = "RMSE (cm)",
         caption = "Vertical line indicates selected feature set size (24)") +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold", size = 14),
          plot.subtitle = element_text(size = 11))

ggsave("out/Figure_Feature_Set_Size_RMSE.png", rmse_plot, 
       width = 8, height = 6, dpi = 300, bg = "white")
cat("RMSE curve saved to 'out/Figure_Feature_Set_Size_RMSE.png'\n")

# Combined plot
combined_plot <- cowplot::plot_grid(
    r2_plot + labs(caption = NULL),
    rmse_plot + labs(caption = NULL),
    ncol = 2,
    labels = c("A", "B")
)

ggsave("out/Figure_Feature_Set_Size_Combined.png", combined_plot, 
       width = 14, height = 6, dpi = 300, bg = "white")
cat("Combined plot saved to 'out/Figure_Feature_Set_Size_Combined.png'\n")

cat("\n=== SCRIPT 11 COMPLETE: Feature set size analysis finished. ===\n")

