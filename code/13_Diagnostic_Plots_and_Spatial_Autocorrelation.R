################################################################################
# SOIL DEPTH ANALYSIS - SCRIPT 13
# DIAGNOSTIC PLOTS AND SPATIAL AUTOCORRELATION ANALYSIS
#
# Purpose:
# 1. Generate standard diagnostic plots for DSM:
#    - Scatter plot of observed vs predicted values
#    - Residual plots (residuals vs predicted, residuals vs observed)
#    - Q-Q plot for residual normality
#    - Histogram of residuals
# 2. Spatial plot of residuals
# 3. Spatial autocorrelation analysis (Moran's I) of residuals
#
# Output:
# - Diagnostic plots
# - Spatial residual map
# - Moran's I analysis results
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
    library(ggplot2)
    library(viridis)
    library(cowplot)
    library(tidyterra)
})

# --- 2. Load Data ---
cat("--- 2. Loading data ---\n")

# Load LOOCV results
loocv_raw_preds <- read_csv("out/final_loocv_raw_predictions.csv", show_col_types = FALSE)
soil_data <- readRDS("out/soil_data_processed.rds")

# Get ensemble model predictions
ensemble_preds <- loocv_raw_preds %>%
    filter(Model == "Ensemble") %>%
    select(Observed, Predicted) %>%
    mutate(Residual = Observed - Predicted)

# Create spatial object for residuals
soil_sf <- st_as_sf(soil_data, coords = c("lon", "lat"), crs = 4326)

# Match residuals to spatial data by index (assuming same order)
if (nrow(soil_sf) == nrow(ensemble_preds)) {
    soil_sf$Residual <- ensemble_preds$Residual
    soil_sf$Predicted <- ensemble_preds$Predicted
} else {
    # If order doesn't match, join by observed value
    soil_sf <- soil_sf %>%
        left_join(ensemble_preds, by = c("dpth" = "Observed"), relationship = "many-to-many") %>%
        group_by(lon, lat) %>%
        slice(1) %>%
        ungroup()
}

# Transform to UTM for distance calculations (Zone 44N for Sri Lanka)
soil_sf_utm <- st_transform(soil_sf, crs = 32644)

cat("Data loaded successfully.\n")

# --- 3. Scatter Plot: Observed vs Predicted ---
cat("--- 3. Creating scatter plot: Observed vs Predicted ---\n")

# Calculate R² and RMSE for annotation
r2_val <- 1 - (sum(ensemble_preds$Residual^2) / sum((ensemble_preds$Observed - mean(ensemble_preds$Observed))^2))
rmse_val <- sqrt(mean(ensemble_preds$Residual^2))
me_val <- mean(ensemble_preds$Residual)

# Create 1:1 line data
max_val <- max(c(ensemble_preds$Observed, ensemble_preds$Predicted), na.rm = TRUE)
min_val <- min(c(ensemble_preds$Observed, ensemble_preds$Predicted), na.rm = TRUE)
one_to_one <- data.frame(x = c(min_val, max_val), y = c(min_val, max_val))

scatter_plot <- ggplot(ensemble_preds, aes(x = Observed, y = Predicted)) +
    geom_point(alpha = 0.6, size = 2, color = "steelblue") +
    geom_line(data = one_to_one, aes(x = x, y = y), linetype = "dashed", color = "red", linewidth = 1) +
    geom_smooth(method = "lm", se = TRUE, color = "darkblue", alpha = 0.3) +
    annotate("text", x = min_val + 0.1 * (max_val - min_val), 
             y = max_val - 0.1 * (max_val - min_val),
             label = paste0("R² = ", round(r2_val, 3), "\nRMSE = ", round(rmse_val, 1), " cm\nME = ", round(me_val, 2), " cm"),
             hjust = 0, vjust = 1, size = 5, fontface = "bold",
             bg = "white", alpha = 0.8) +
    labs(title = "Observed vs Predicted Soil Depth",
         subtitle = "Ensemble model (LOOCV)",
         x = "Observed Depth (cm)", y = "Predicted Depth (cm)") +
    theme_minimal(base_size = 14) +
    theme(plot.title = element_text(face = "bold"),
          panel.grid.minor = element_blank()) +
    coord_fixed(ratio = 1)

ggsave("out/Figure_Diagnostic_Scatter_Observed_vs_Predicted.png", scatter_plot, 
       width = 8, height = 8, dpi = 300, bg = "white")

cat("Scatter plot saved.\n")

# --- 4. Residual Plots ---
cat("--- 4. Creating residual diagnostic plots ---\n")

# 4a. Residuals vs Predicted
resid_vs_pred <- ggplot(ensemble_preds, aes(x = Predicted, y = Residual)) +
    geom_point(alpha = 0.6, size = 2, color = "steelblue") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
    geom_smooth(method = "loess", se = TRUE, color = "darkblue", alpha = 0.3) +
    labs(title = "Residuals vs Predicted Values",
         subtitle = "Checking for heteroscedasticity and non-linearity",
         x = "Predicted Depth (cm)", y = "Residual (cm)") +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold", size = 12))

# 4b. Residuals vs Observed
resid_vs_obs <- ggplot(ensemble_preds, aes(x = Observed, y = Residual)) +
    geom_point(alpha = 0.6, size = 2, color = "steelblue") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
    geom_smooth(method = "loess", se = TRUE, color = "darkblue", alpha = 0.3) +
    labs(title = "Residuals vs Observed Values",
         subtitle = "Checking for systematic bias",
         x = "Observed Depth (cm)", y = "Residual (cm)") +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold", size = 12))

# 4c. Q-Q Plot for normality
qq_plot <- ggplot(ensemble_preds, aes(sample = Residual)) +
    stat_qq(alpha = 0.6, size = 2, color = "steelblue") +
    stat_qq_line(color = "red", linetype = "dashed", linewidth = 1) +
    labs(title = "Q-Q Plot of Residuals",
         subtitle = "Checking for normality",
         x = "Theoretical Quantiles", y = "Sample Quantiles") +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold", size = 12))

# 4d. Histogram of residuals
hist_plot <- ggplot(ensemble_preds, aes(x = Residual)) +
    geom_histogram(aes(y = after_stat(density)), bins = 20, fill = "steelblue", alpha = 0.7, color = "white") +
    geom_density(color = "darkblue", linewidth = 1) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
    stat_function(fun = dnorm, args = list(mean = mean(ensemble_preds$Residual), 
                                           sd = sd(ensemble_preds$Residual)),
                  color = "orange", linetype = "dashed", linewidth = 1) +
    labs(title = "Distribution of Residuals",
         subtitle = paste0("Mean = ", round(mean(ensemble_preds$Residual), 2), 
                          " cm, SD = ", round(sd(ensemble_preds$Residual), 2), " cm"),
         x = "Residual (cm)", y = "Density") +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold", size = 12))

# Combine residual plots
residual_plots <- plot_grid(
    resid_vs_pred, resid_vs_obs,
    qq_plot, hist_plot,
    ncol = 2, labels = c("A", "B", "C", "D")
)

ggsave("out/Figure_Diagnostic_Residual_Plots.png", residual_plots, 
       width = 14, height = 10, dpi = 300, bg = "white")

cat("Residual diagnostic plots saved.\n")

# --- 5. Spatial Plot of Residuals ---
cat("--- 5. Creating spatial plot of residuals ---\n")

# Create color scale symmetric around zero
max_abs_resid <- max(abs(ensemble_preds$Residual), na.rm = TRUE)
resid_breaks <- seq(-max_abs_resid, max_abs_resid, length.out = 11)

spatial_residual_plot <- ggplot() +
    geom_sf(data = soil_sf, aes(color = Residual, size = abs(Residual)), 
            alpha = 0.8, show.legend = "point") +
    scale_color_gradient2(name = "Residual\n(cm)", 
                         low = "blue", mid = "white", high = "red",
                         midpoint = 0,
                         limits = c(-max_abs_resid, max_abs_resid),
                         guide = guide_colorbar(title.position = "top")) +
    scale_size_continuous(name = "|Residual|\n(cm)",
                         range = c(2, 6),
                         guide = guide_legend(title.position = "top")) +
    labs(title = "Spatial Distribution of Residuals",
         subtitle = "Ensemble model (LOOCV) - Red = overprediction, Blue = underprediction",
         x = "Longitude", y = "Latitude") +
    coord_sf(expand = FALSE) +
    theme_minimal(base_size = 14) +
    theme(plot.title = element_text(face = "bold"),
          legend.position = "right")

ggsave("out/Figure_Diagnostic_Spatial_Residuals.png", spatial_residual_plot, 
       width = 10, height = 10, dpi = 300, bg = "white")

cat("Spatial residual plot saved.\n")

# --- 6. Spatial Autocorrelation Analysis (Moran's I) ---
cat("--- 6. Conducting spatial autocorrelation analysis (Moran's I) ---\n")

# Get residuals from spatial object
residuals <- as.numeric(soil_sf_utm$Residual)
valid_idx <- !is.na(residuals)
residuals <- residuals[valid_idx]

# Calculate distance matrix using UTM coordinates
coords_utm <- st_coordinates(soil_sf_utm)[valid_idx, ]
n <- length(residuals)

# Calculate pairwise distances
dist_matrix <- as.matrix(dist(coords_utm))

# Create inverse distance weights matrix (with threshold to avoid infinite weights)
# Use k=5 nearest neighbors approach
k <- min(5, n - 1)  # Ensure k is not larger than n-1
weights_matrix <- matrix(0, nrow = n, ncol = n)

for (i in 1:n) {
    # Get k nearest neighbors (excluding self)
    distances <- dist_matrix[i, ]
    distances[i] <- Inf  # Exclude self
    k_nearest <- order(distances)[1:k]
    
    # Set weights (1/distance, normalized)
    weights <- 1 / (distances[k_nearest] + 1)  # Add 1 to avoid division by zero
    weights <- weights / sum(weights)  # Normalize
    weights_matrix[i, k_nearest] <- weights
}

# Calculate Moran's I manually
residuals_centered <- residuals - mean(residuals)
numerator <- 0
denominator <- sum(residuals_centered^2)

# Calculate sum of weights (S0)
s0 <- sum(weights_matrix)

# Calculate numerator: sum of weighted products of centered residuals
for (i in 1:n) {
    for (j in 1:n) {
        if (i != j && weights_matrix[i, j] > 0) {
            numerator <- numerator + weights_matrix[i, j] * residuals_centered[i] * residuals_centered[j]
        }
    }
}

# Moran's I formula
if (denominator > 0 && s0 > 0) {
    morans_i <- (n / s0) * (numerator / denominator)
} else {
    morans_i <- NA
    cat("Warning: Cannot calculate Moran's I (denominator or s0 is zero)\n")
}

# Calculate expected value and variance under null hypothesis
expected_i <- -1 / (n - 1)

# Calculate variance (simplified formula for row-standardized weights)
s1 <- sum(weights_matrix^2)
s2 <- sum((rowSums(weights_matrix) + colSums(weights_matrix))^2)
n_sq <- n^2
s0 <- sum(weights_matrix)

# Variance calculation
var_i <- (n_sq * s1 - n * s2 + 3 * s0^2) / ((n^2 - 1) * s0^2) - expected_i^2

# Calculate z-score and p-value
if (!is.na(morans_i) && var_i > 0) {
    z_score <- (morans_i - expected_i) / sqrt(var_i)
    p_value <- 2 * (1 - pnorm(abs(z_score)))
} else {
    z_score <- NA
    p_value <- NA
}

cat("\n--- Moran's I Test Results ---\n")
cat("Moran's I statistic:", round(morans_i, 4), "\n")
cat("Expected value (under null):", round(expected_i, 4), "\n")
cat("Variance:", round(var_i, 6), "\n")
cat("Standard deviate (z-score):", round(z_score, 4), "\n")
cat("p-value:", format.pval(p_value, digits = 4), "\n")

# Interpretation
if (!is.na(p_value)) {
    if (p_value < 0.05) {
        if (morans_i > 0) {
            cat("\nInterpretation: Significant positive spatial autocorrelation detected.\n")
            cat("Residuals are spatially clustered (similar residuals near each other).\n")
            cat("This suggests that spatial structure remains unexplained by the model.\n")
        } else {
            cat("\nInterpretation: Significant negative spatial autocorrelation detected.\n")
            cat("Residuals are spatially dispersed (dissimilar residuals near each other).\n")
        }
    } else {
        cat("\nInterpretation: No significant spatial autocorrelation detected.\n")
        cat("Residuals appear to be spatially independent.\n")
    }
} else {
    cat("\nInterpretation: Could not calculate significance test.\n")
}

# Save Moran's I results
moran_results <- data.frame(
    Statistic = c("Moran's I", "Expected I", "Variance", "Z-score", "p-value"),
    Value = c(morans_i, expected_i, var_i, z_score, p_value)
)

write_csv(moran_results, "out/morans_i_residuals.csv")

# Create spatially lagged residuals for scatter plot
lagged_residuals <- numeric(n)
for (i in 1:n) {
    lagged_residuals[i] <- sum(weights_matrix[i, ] * residuals)
}

moran_scatter_data <- data.frame(
    Residual = residuals,
    Lagged_Residual = lagged_residuals
)

moran_scatter <- ggplot(moran_scatter_data, aes(x = Residual, y = Lagged_Residual)) +
    geom_point(alpha = 0.6, size = 2, color = "steelblue") +
    geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "dashed", alpha = 0.3) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "gray") +
    geom_vline(xintercept = 0, linetype = "dotted", color = "gray") +
    annotate("text", x = min(moran_scatter_data$Residual, na.rm = TRUE) + 0.1 * diff(range(moran_scatter_data$Residual, na.rm = TRUE)),
             y = max(moran_scatter_data$Lagged_Residual, na.rm = TRUE) - 0.1 * diff(range(moran_scatter_data$Lagged_Residual, na.rm = TRUE)),
             label = ifelse(!is.na(morans_i) && !is.na(p_value),
                          paste0("Moran's I = ", round(morans_i, 4), 
                               "\np-value = ", format.pval(p_value, digits = 4)),
                          "Moran's I calculation failed"),
             hjust = 0, vjust = 1, size = 4, fontface = "bold",
             bg = "white", alpha = 0.8) +
    labs(title = "Moran's I Scatter Plot",
         subtitle = "Residuals vs Spatially Lagged Residuals (k=5 nearest neighbors)",
         x = "Residual (cm)", y = "Spatially Lagged Residual (cm)") +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold", size = 12))

ggsave("out/Figure_Diagnostic_Morans_I_Scatter.png", moran_scatter, 
       width = 8, height = 8, dpi = 300, bg = "white")

cat("Moran's I scatter plot saved.\n")

# --- 7. Summary Statistics ---
cat("\n--- 7. Summary Statistics ---\n")
cat("Residual Statistics:\n")
cat("  Mean:", round(mean(ensemble_preds$Residual), 3), "cm\n")
cat("  Standard Deviation:", round(sd(ensemble_preds$Residual), 3), "cm\n")
cat("  Min:", round(min(ensemble_preds$Residual), 3), "cm\n")
cat("  Max:", round(max(ensemble_preds$Residual), 3), "cm\n")
cat("  Range:", round(max(ensemble_preds$Residual) - min(ensemble_preds$Residual), 3), "cm\n")

# Shapiro-Wilk test for normality
shapiro_test <- shapiro.test(ensemble_preds$Residual)
cat("\nShapiro-Wilk Test for Normality:\n")
cat("  W =", round(shapiro_test$statistic, 4), "\n")
cat("  p-value =", format.pval(shapiro_test$p.value, digits = 4), "\n")
if (shapiro_test$p.value < 0.05) {
    cat("  Interpretation: Residuals are NOT normally distributed (p < 0.05)\n")
} else {
    cat("  Interpretation: Residuals appear to be normally distributed (p >= 0.05)\n")
}

# Save summary statistics
summary_stats <- data.frame(
    Statistic = c("Mean", "SD", "Min", "Max", "Range", 
                  "Shapiro-Wilk W", "Shapiro-Wilk p-value",
                  "Moran's I", "Moran's I p-value"),
    Value = c(mean(ensemble_preds$Residual), sd(ensemble_preds$Residual),
              min(ensemble_preds$Residual), max(ensemble_preds$Residual),
              max(ensemble_preds$Residual) - min(ensemble_preds$Residual),
              shapiro_test$statistic, shapiro_test$p.value,
              morans_i, p_value)
)

write_csv(summary_stats, "out/diagnostic_summary_statistics.csv")

cat("\n=== SCRIPT 13 COMPLETE: Diagnostic plots and spatial autocorrelation analysis finished. ===\n")

