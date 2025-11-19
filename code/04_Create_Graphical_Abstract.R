################################################################################
# SOIL DEPTH ANALYSIS - SCRIPT 4 of 4
# CREATE GRAPHICAL ABSTRACT
#
# Purpose:
# 1. Generate a publication-quality graphical abstract summarizing the study.
#
# Output:
# - A graphical abstract saved as 'out/graphical_abstract.png'.
################################################################################

# --- 1. Environment Setup ---
cat("--- 1. Setting up the environment ---\n")

# Clear workspace
rm(list = ls())

# Set working directory
setwd("/Users/ej/CFFRC/04-Research/Soil/depth SLK")

# Load required libraries
suppressPackageStartupMessages({
    library(ggplot2)
    library(sf)
    library(terra)
    library(cowplot)
    library(tidyterra)
})

# --- 2. Load Data ---
cat("--- 2. Loading data ---\n")
final_map <- rast("out/Final_Ensemble_Soil_Depth_Map.tif")
soil_data <- readRDS("out/soil_data_processed.rds")
soil_sf <- st_as_sf(soil_data, coords = c("lon", "lat"), crs = 4326)

# --- 3. Create Panels ---
cat("--- 3. Creating panels for the graphical abstract ---\n")

# Panel 1: Inputs
panel1 <- ggplot() +
  geom_spatraster(data = final_map, fill = "gray80", alpha = 0.5) +
  geom_sf(data = soil_sf, color = "red", size = 2) +
  labs(title = "Inputs", subtitle = "96 soil samples + 247 covariates") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))

# Panel 2: Methods
panel2 <- ggplot() +
  annotate("text", x = 0.5, y = 0.9, label = "Methods", fontface = "bold", size = 6) +
  annotate("text", x = 0.5, y = 0.7, label = "Feature Selection -> RF, XGBoost, Cubist, SVM -> Ensemble Model", size = 4) +
  theme_void()

# Panel 3: Outputs
panel3 <- ggplot() +
  geom_spatraster(data = final_map) +
  scale_fill_viridis_c(name = "Soil Depth (cm)", na.value = "transparent") +
  labs(title = "Outputs", subtitle = "R² = 0.30, RMSE = 33.0 cm") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none")

# --- 4. Combine Panels ---
cat("--- 4. Combining panels into the final graphical abstract ---\n")
graphical_abstract <- plot_grid(panel1, panel2, panel3, ncol = 3)

# --- 5. Save Graphical Abstract ---
cat("--- 5. Saving the graphical abstract ---\n")
ggsave("out/graphical_abstract.png", graphical_abstract, width = 12, height = 4, dpi = 300, bg = "white")

cat("\n=== SCRIPT 04 COMPLETE: Graphical abstract created. ===\n")







