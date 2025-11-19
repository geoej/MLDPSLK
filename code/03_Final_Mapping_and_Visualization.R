################################################################################
# SOIL DEPTH ANALYSIS - SCRIPT 3 of 3
# FINAL MAPPING AND VISUALIZATION
#
# Purpose:
# 1. Train the final, best-performing model (Stacked Ensemble) on the full dataset.
# 2. Generate the definitive high-resolution soil depth map.
# 3. Create a comprehensive set of publication-quality figures for the manuscript.
#
# Output:
# - The final soil depth map ('out/Final_Ensemble_Soil_Depth_Map.tif').
# - A complete set of figures (PNG and PDF) in the '/out' directory.
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
    library(caretEnsemble)
    library(ggplot2)
    library(viridis)
    library(cowplot)
    library(tidyterra)
})

# --- 2. Load Final Data Artifacts ---
cat("--- 2. Loading final data artifacts from Script 02 ---\n")
soil_data <- readRDS("out/soil_data_processed.rds")
top_24_vars <- read_csv("out/top_24_covariates_final.csv", show_col_types = FALSE)$variable
model_list <- readRDS("out/final_model_list.rds")
ensemble_meta_model <- readRDS("out/final_ensemble_model.rds")
loocv_summary_final <- read_csv("out/final_loocv_summary.csv", show_col_types = FALSE)
loocv_raw_preds <- read_csv("out/final_loocv_raw_predictions.csv", show_col_types = FALSE)


# --- 3. Train Final Ensemble Model & Generate Map ---
cat("--- 3. Training final ensemble model and generating map ---\n")

# Decision: The final model is a stacked ensemble. We re-train it on the full
# dataset to ensure it learns from all available data before making the final map.
cat("Training the final stacked ensemble on the full dataset...\n")
set.seed(42)
final_ensemble <- caretStack(
    model_list,
    method = "glm",
    trControl = trainControl(method = "boot", number = 25)
)

# Prepare the raster stack with the top 24 covariates.
cat("Loading and stacking the 24 selected covariate rasters...\n")
covariates_dir <- "/Users/ej/CFFRC/04-Research/Soil/SLK soil data/Covariates/ISRIC covariates /isric_gsp/Covs"
raster_files <- file.path(covariates_dir, paste0(top_24_vars, ".tif"))
# Ensure all files exist before creating the stack
existing_files <- raster_files[file.exists(raster_files)]
reduced_stack <- rast(existing_files)
names(reduced_stack) <- top_24_vars[file.exists(raster_files)]

# Generate the final prediction map.
cat("Generating final prediction map...\n")
final_map <- predict(reduced_stack, final_ensemble, na.rm = TRUE, fun = function(model, ...) expm1(predict(model, ...)))
names(final_map) <- "Ensemble_Depth_cm"
writeRaster(final_map, "out/Final_Ensemble_Soil_Depth_Map.tif", overwrite = TRUE)
cat("Final map saved to 'out/Final_Ensemble_Soil_Depth_Map.tif'.\n")


# --- 4. Generate All Manuscript Figures ---
cat("--- 4. Generating all manuscript figures ---\n")

soil_sf <- st_as_sf(soil_data, coords = c("lon", "lat"), crs = 4326)

# FIGURE 1: Study Area Map
study_area_map <- ggplot() +
    geom_spatraster(data = final_map) +
    scale_fill_viridis_c(na.value = "transparent", guide = "none") +
    geom_sf(data = soil_sf, color = "red", fill = "white", shape = 21, size = 3, stroke = 1) +
    labs(title = "Study Area and Soil Sample Locations", subtitle = "Sri Lanka (n = 96)",
         x = "Longitude", y = "Latitude") +
    theme_minimal(base_size = 14) + coord_sf(expand = FALSE) +
    theme(plot.title = element_text(face="bold"))
ggsave("out/Figure1_Study_Area_Map.png", study_area_map, width = 8, height = 10, dpi = 300, bg="white")

# FIGURE 2: Model Performance Comparison
loocv_residuals <- loocv_raw_preds %>%
  mutate(Residual = Predicted - Observed,
         Model = factor(Model, levels = c("Ensemble", "RF", "SVM", "XGBoost", "Cubist")))
loocv_summary_final$Label <- paste0("R² = ", round(loocv_summary_final$R2, 3), "\nRMSE = ", round(loocv_summary_final$RMSE, 2))

performance_plot <- ggplot(loocv_residuals, aes(x = Model, y = Residual)) +
  geom_boxplot(aes(fill = Model), alpha = 0.7, show.legend = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_text(data = loocv_summary_final, aes(x = Model, y = 60, label = Label), 
            fontface = "bold", size = 4, vjust = 0) +
  scale_fill_viridis_d() +
  labs(title = "Model Performance Comparison (Leave-One-Out Cross-Validation)",
       subtitle = "Distribution of prediction residuals", x = "Model", y = "Residual (cm)") +
  theme_minimal(base_size = 14) + theme(plot.title = element_text(face="bold")) + ylim(-75, 75)
ggsave("out/Figure2_Model_Performance_Comparison.png", performance_plot, width = 12, height = 8, dpi = 300, bg="white")

# FIGURE 3: Final Prediction Map
final_map_plot <- ggplot() +
  geom_spatraster(data = final_map) +
  geom_sf(data = soil_sf, color = "black", fill = "white", shape = 21, size = 2, stroke=1) +
  scale_fill_viridis_c(name = "Soil Depth (cm)", limits = c(0, 200), oob = scales::squish, direction = -1, na.value="transparent") +
  labs(title = "Final Predicted Soil Depth of Sri Lanka",
       subtitle = "Based on a stacked ensemble of four machine learning models",
       x = "Longitude", y = "Latitude") +
  coord_sf(expand = FALSE) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "right", plot.title = element_text(face="bold"))
ggsave("out/Figure3_Final_Ensemble_Map.png", final_map_plot, width = 8, height = 10, dpi = 300, bg="white")


# FIGURE 4 & 5: Shangguan et al. Comparison
shangguan_map_raw <- rast("external data/BDRICM_M_250m_ll.tif")
shangguan_map <- resample(crop(shangguan_map_raw, final_map), final_map, method="bilinear")

# Distribution Plot
our_values <- as.data.frame(final_map, na.rm = TRUE) %>% mutate(Source = "Our Ensemble Model")
shangguan_values <- as.data.frame(shangguan_map, na.rm = TRUE) %>% mutate(Source = "Shangguan et al. (2017)")
names(our_values)[1] <- "Depth"; names(shangguan_values)[1] <- "Depth"
combined_distributions <- bind_rows(our_values, shangguan_values)
distribution_plot <- ggplot(combined_distributions, aes(x = Depth, fill = Source)) +
  geom_density(alpha = 0.7) +
  scale_fill_manual(values = c("Our Ensemble Model" = "#1f78b4", "Shangguan et al. (2017)" = "#e31a1c")) +
  labs(title = "Distribution of Predicted Soil Depths",
       subtitle = "Comparison between the locally-calibrated ensemble and a global model",
       x = "Predicted Soil Depth (cm)", y = "Density", fill = "Data Source") +
  theme_minimal(base_size = 14) + theme(legend.position = "bottom", plot.title = element_text(face = "bold")) + xlim(0, 250)
ggsave("out/Figure4_Distribution_Comparison.png", distribution_plot, width = 10, height = 7, dpi = 300, bg="white")

# Accuracy Plot
shangguan_at_points <- terra::extract(shangguan_map_raw, vect(soil_sf))[,2]
our_ensemble_preds <- loocv_raw_preds %>% filter(Model == "Ensemble") %>% pull(Predicted)
accuracy_df <- data.frame(Observed = soil_sf$dpth, `Our Ensemble Model` = our_ensemble_preds, `Shangguan et al. (2017)` = shangguan_at_points) %>%
  tidyr::pivot_longer(cols = -Observed, names_to = "Model", values_to = "Predicted")
r2_scores <- accuracy_df %>% group_by(Model) %>% summarise(R2 = 1 - (sum((Observed - Predicted)^2, na.rm=T) / sum((Observed - mean(Observed, na.rm=T))^2, na.rm=T)), Label = paste("R² =", round(R2, 3))) %>% mutate(x_pos = 50, y_pos = 200)

accuracy_plot <- ggplot(accuracy_df, aes(x = Observed, y = Predicted)) +
  geom_point(alpha = 0.7, color = "blue") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", linewidth = 1) +
  geom_text(data = r2_scores, aes(x = x_pos, y = y_pos, label = Label), size = 5, fontface = "bold") +
  facet_wrap(~Model) +
  labs(title = "Head-to-Head Model Accuracy at 96 Sample Locations",
       subtitle = "Comparing model predictions against measured soil depth",
       x = "Observed Soil Depth (cm)", y = "Predicted Soil Depth (cm)") +
  coord_equal(xlim = c(0, 220), ylim = c(0, 220), expand = FALSE) +
  theme_minimal(base_size = 14) + theme(strip.text = element_text(face="bold", size=12), plot.title = element_text(face="bold"))
ggsave("out/Figure5_Accuracy_Comparison.png", accuracy_plot, width = 12, height = 7, dpi = 300, bg="white")


cat("\n=== SCRIPT 03 COMPLETE: All final outputs have been generated. ===\n")

