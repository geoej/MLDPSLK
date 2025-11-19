################################################################################
# SOIL DEPTH ANALYSIS - SCRIPT 5
# QUANTITATIVE COMPARISON WITH GLOBAL PRODUCTS
#
# Purpose:
# 1. Extract predictions from global product (Shangguan et al. 2017) at validation points
# 2. Calculate comprehensive comparison metrics (R², RMSE, ME)
# 3. Generate a comparison table for the manuscript
#
# Output:
# - Comparison metrics table ('out/global_products_comparison.csv')
# - Comparison metrics table in LaTeX format ('out/global_products_comparison.tex')
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
})

# --- 2. Load Data ---
cat("--- 2. Loading data ---\n")

# Load soil data with observed values
soil_data <- readRDS("out/soil_data_processed.rds")
soil_sf <- st_as_sf(soil_data, coords = c("lon", "lat"), crs = 4326)

# Load LOOCV predictions from our ensemble model
loocv_raw_preds <- read_csv("out/final_loocv_raw_predictions.csv", show_col_types = FALSE)
our_ensemble_preds <- loocv_raw_preds %>% 
  filter(Model == "Ensemble") %>% 
  pull(Predicted)

# Observed values
observed <- soil_data$dpth

# --- 3. Extract Predictions from Global Product ---
cat("--- 3. Extracting predictions from global product ---\n")

# Shangguan et al. (2017) - BDRICM (Bedrock Depth)
cat("Extracting Shangguan et al. (2017) predictions...\n")
shangguan_map <- rast("external data/BDRICM_M_250m_ll.tif")
shangguan_preds <- terra::extract(shangguan_map, vect(soil_sf))[,2]

# --- 4. Calculate Performance Metrics ---
cat("--- 4. Calculating performance metrics ---\n")

# Function to calculate metrics
calculate_metrics <- function(observed, predicted, model_name) {
  # Remove NA values
  valid_idx <- !is.na(observed) & !is.na(predicted)
  obs_clean <- observed[valid_idx]
  pred_clean <- predicted[valid_idx]
  
  if (length(obs_clean) == 0) {
    return(data.frame(
      Model = model_name,
      n = 0,
      R2 = NA,
      RMSE = NA,
      ME = NA
    ))
  }
  
  # Calculate metrics
  n <- length(obs_clean)
  ss_res <- sum((obs_clean - pred_clean)^2)
  ss_tot <- sum((obs_clean - mean(obs_clean))^2)
  r2 <- 1 - (ss_res / ss_tot)
  rmse <- sqrt(mean((obs_clean - pred_clean)^2))
  me <- mean(pred_clean - obs_clean)
  
  return(data.frame(
    Model = model_name,
    n = n,
    R2 = round(r2, 3),
    RMSE = round(rmse, 1),
    ME = round(me, 1)
  ))
}

# Calculate metrics for all models
metrics_our <- calculate_metrics(observed, our_ensemble_preds, "Our Ensemble Model")
metrics_shangguan <- calculate_metrics(observed, shangguan_preds, "Shangguan et al. (2017)")

# Combine all metrics
comparison_table <- bind_rows(
  metrics_our,
  metrics_shangguan
)

# --- 5. Save Results ---
cat("--- 5. Saving comparison results ---\n")

# Save as CSV
write_csv(comparison_table, "out/global_products_comparison.csv")
cat("Comparison table saved to 'out/global_products_comparison.csv'\n")

# Save as LaTeX table
library(knitr)
library(kableExtra)

latex_table <- comparison_table %>%
  kable(format = "latex", booktabs = TRUE, 
        caption = "Quantitative comparison of locally-calibrated ensemble model with global soil depth product (Shangguan et al. 2017) at 96 validation points.",
        label = "tab:global_comparison",
        col.names = c("Model", "n", "R²", "RMSE (cm)", "ME (cm)")) %>%
  kable_styling(latex_options = c("striped", "hold_position"))

writeLines(latex_table, "out/global_products_comparison.tex")
cat("LaTeX table saved to 'out/global_products_comparison.tex'\n")

# Print summary
cat("\n=== COMPARISON SUMMARY ===\n")
print(comparison_table)

# --- 6. Create Visualization ---
cat("--- 6. Creating comparison visualization ---\n")

# Prepare data for plotting
comparison_df <- data.frame(
  Observed = rep(observed, 2),
  Predicted = c(our_ensemble_preds, shangguan_preds),
  Model = rep(c("Our Ensemble Model", "Shangguan et al. (2017)"), 
              each = length(observed))
) %>%
  filter(!is.na(Observed) & !is.na(Predicted))

# Calculate R² for each model for annotation
r2_annotations <- comparison_df %>%
  group_by(Model) %>%
  summarise(
    R2 = round(1 - (sum((Observed - Predicted)^2) / sum((Observed - mean(Observed))^2)), 3),
    RMSE = round(sqrt(mean((Observed - Predicted)^2)), 1),
    .groups = "drop"
  ) %>%
  mutate(
    Label = paste0("R² = ", R2, "\nRMSE = ", RMSE, " cm"),
    x_pos = 30,
    y_pos = 200
  )

# Create comparison plot
comparison_plot <- ggplot(comparison_df, aes(x = Observed, y = Predicted)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", linewidth = 1) +
  geom_text(data = r2_annotations, aes(x = x_pos, y = y_pos, label = Label), 
            size = 4, fontface = "bold", hjust = 0) +
  facet_wrap(~Model, ncol = 2) +
  labs(
    title = "Quantitative Comparison with Global Products",
    subtitle = "Model predictions vs. observed soil depth at 96 validation points",
    x = "Observed Soil Depth (cm)",
    y = "Predicted Soil Depth (cm)"
  ) +
  coord_equal(xlim = c(0, 220), ylim = c(0, 220), expand = FALSE) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold", size = 11),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 12)
  )

ggsave("out/Figure_Global_Products_Comparison.png", comparison_plot, 
       width = 10, height = 5, dpi = 300, bg = "white")
cat("Comparison plot saved to 'out/Figure_Global_Products_Comparison.png'\n")

cat("\n=== SCRIPT 05 COMPLETE: Global products comparison finished. ===\n")

