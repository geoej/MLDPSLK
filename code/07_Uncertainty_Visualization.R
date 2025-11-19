################################################################################
# SOIL DEPTH ANALYSIS - SCRIPT 7
# UNCERTAINTY MAP VISUALIZATION
#
# Purpose:
# 1. Create publication-quality visualizations of uncertainty maps
# 2. Generate combined maps showing predictions with uncertainty
#
# Output:
# - Uncertainty visualization figures
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
    library(sf)
    library(terra)
    library(ggplot2)
    library(viridis)
    library(cowplot)
    library(tidyterra)
})

# --- 2. Load Data ---
cat("--- 2. Loading maps and data ---\n")
final_map <- rast("out/Final_Ensemble_Soil_Depth_Map.tif")
uncertainty_sd <- rast("out/uncertainty_map_sd.tif")
uncertainty_pi_lower <- rast("out/uncertainty_map_pi_lower.tif")
uncertainty_pi_upper <- rast("out/uncertainty_map_pi_upper.tif")
soil_data <- readRDS("out/soil_data_processed.rds")
soil_sf <- st_as_sf(soil_data, coords = c("lon", "lat"), crs = 4326)

# --- 3. Create Uncertainty Visualizations ---
cat("--- 3. Creating uncertainty visualizations ---\n")

# Figure 1: Prediction Standard Deviation Map
uncertainty_sd_plot <- ggplot() +
    geom_spatraster(data = uncertainty_sd) +
    geom_sf(data = soil_sf, color = "red", fill = "white", shape = 21, size = 2, stroke = 1) +
    scale_fill_viridis_c(name = "SD (cm)", na.value = "transparent", direction = -1) +
    labs(title = "Prediction Uncertainty: Standard Deviation",
         subtitle = "Based on spatial cross-validation residuals",
         x = "Longitude", y = "Latitude") +
    coord_sf(expand = FALSE) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "right", plot.title = element_text(face = "bold"))

ggsave("out/Figure_Uncertainty_SD.png", uncertainty_sd_plot, width = 8, height = 10, dpi = 300, bg = "white")

# Figure 2: Prediction Intervals (95% CI)
# Create a combined plot showing lower and upper bounds
pi_lower_plot <- ggplot() +
    geom_spatraster(data = uncertainty_pi_lower) +
    geom_sf(data = soil_sf, color = "red", fill = "white", shape = 21, size = 1.5, stroke = 0.8) +
    scale_fill_viridis_c(name = "Lower\nBound (cm)", na.value = "transparent", limits = c(0, 200), oob = scales::squish) +
    labs(title = "95% Prediction Interval: Lower Bound",
         x = "Longitude", y = "Latitude") +
    coord_sf(expand = FALSE) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "right", plot.title = element_text(face = "bold", size = 12))

pi_upper_plot <- ggplot() +
    geom_spatraster(data = uncertainty_pi_upper) +
    geom_sf(data = soil_sf, color = "red", fill = "white", shape = 21, size = 1.5, stroke = 0.8) +
    scale_fill_viridis_c(name = "Upper\nBound (cm)", na.value = "transparent", limits = c(0, 200), oob = scales::squish) +
    labs(title = "95% Prediction Interval: Upper Bound",
         x = "Longitude", y = "Latitude") +
    coord_sf(expand = FALSE) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "right", plot.title = element_text(face = "bold", size = 12))

# Combine into a single figure
pi_combined <- plot_grid(pi_lower_plot, pi_upper_plot, ncol = 2, labels = c("A", "B"))
ggsave("out/Figure_Uncertainty_Prediction_Intervals.png", pi_combined, width = 16, height = 8, dpi = 300, bg = "white")

# Figure 3: Combined prediction and uncertainty
prediction_plot <- ggplot() +
    geom_spatraster(data = final_map) +
    geom_sf(data = soil_sf, color = "black", fill = "white", shape = 21, size = 2, stroke = 1) +
    scale_fill_viridis_c(name = "Depth (cm)", na.value = "transparent", limits = c(0, 200), oob = scales::squish, direction = -1) +
    labs(title = "Predicted Soil Depth",
         x = "Longitude", y = "Latitude") +
    coord_sf(expand = FALSE) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "right", plot.title = element_text(face = "bold", size = 12))

# Create a 2x2 grid showing prediction and uncertainty
combined_uncertainty_figure <- plot_grid(
    prediction_plot, uncertainty_sd_plot,
    ncol = 2, labels = c("A", "B"),
    rel_widths = c(1, 1)
)

ggsave("out/Figure_Prediction_and_Uncertainty.png", combined_uncertainty_figure, width = 16, height = 8, dpi = 300, bg = "white")

cat("Uncertainty visualizations saved.\n")
cat("\n=== SCRIPT 07 COMPLETE: Uncertainty visualization finished. ===\n")








