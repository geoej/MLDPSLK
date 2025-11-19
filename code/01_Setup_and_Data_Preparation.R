################################################################################
# SOIL DEPTH ANALYSIS - SCRIPT 1 of 3
# SETUP AND DATA PREPARATION
#
# Purpose:
# 1. Install and load all required R packages.
# 2. Load the master dataset (FinalData.csv).
# 3. Perform initial data cleaning and verification.
# 4. Transform the target variable (soil depth) to improve model performance.
# 5. Create spatial objects and project the data to a suitable CRS for analysis.
#
# Output:
# - A clean, processed data frame saved as 'out/soil_data_processed.rds'.
# - Exploratory plots saved to the '/out' directory.
################################################################################

# --- 1. Environment Setup ---
cat("--- 1. Setting up the environment ---\n")

# Clear workspace
rm(list = ls())

# Set working directory
setwd("/Users/ej/CFFRC/04-Research/Soil/depth SLK")

# --- 2. Package Installation and Loading ---
cat("--- 2. Installing and loading packages ---\n")

# List of required packages
packages <- c("dplyr", "readr", "tidyr", "sf", "terra", "sp", "gstat", "caret", 
              "caretEnsemble", "randomForest", "xgboost", "Cubist", "kernlab", 
              "ggplot2", "viridis", "cowplot", "tidyterra", "knitr", "kableExtra")

# Install packages if they are not already installed
install_if_missing <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    cat(paste("Installing", pkg, "...\n"))
    install.packages(pkg, dependencies = TRUE, repos = "https://cloud.r-project.org/")
  }
}
sapply(packages, install_if_missing)

# Load all packages
suppressPackageStartupMessages({
    lapply(packages, library, character.only = TRUE)
})

cat("All packages are loaded.\n")


# --- 3. Data Ingestion and Initial Verification ---
cat("--- 3. Loading and verifying the master dataset ---\n")

# Decision: Load the master regression matrix which contains all covariates.
soil_data <- read_csv("in/FinalData.csv", show_col_types = FALSE)

# Rename columns to match the script's expectations
soil_data <- soil_data %>%
  rename(dpth = Dpth___, lat = Latitud, lon = Longitd)

# Remove rows with NA values
soil_data <- na.omit(soil_data)

cat("Initial data verification:\n")
cat(paste(" - Observations:", nrow(soil_data), "\n"))
cat(paste(" - Variables:", ncol(soil_data), "\n"))
cat(paste(" - Total NA values:", sum(is.na(soil_data)), "\n"))


# --- 4. Target Variable Transformation ---
cat("--- 4. Transforming the target variable (soil depth) ---\n")

# Decision: The soil depth variable ('dpth') is right-skewed. To meet the assumptions
# of many statistical models and improve performance, a log transformation is applied.
# We use log1p (log(x+1)) to handle any potential zero values.
soil_data$log_dpth <- log1p(soil_data$dpth)

# Visualization: Create a plot to show the effect of the transformation.
distribution_transform_plot <- ggplot(tidyr::pivot_longer(soil_data, cols = c(dpth, log_dpth), names_to = "Variable", values_to = "Value"), 
                                      aes(x = Value)) +
  geom_histogram(aes(y = ..density..), bins = 20, fill = "steelblue", alpha = 0.7) +
  geom_density(color = "red", linewidth = 1) +
  facet_wrap(~Variable, scales = "free") +
  labs(title = "Distribution of Soil Depth Before and After Log Transformation",
       x = "Value", y = "Density") +
  theme_minimal()

ggsave("out/A1_Depth_Distribution_Transformation.png", distribution_transform_plot, width = 10, height = 5, dpi = 300, bg="white")
cat("Transformation plot saved to 'out/A1_Depth_Distribution_Transformation.png'.\n")


# --- 5. Spatial Data Preparation ---
cat("--- 5. Preparing spatial data objects ---\n")

# Decision: For spatial analysis, the data needs to be converted into a formal
# spatial object with a defined Coordinate Reference System (CRS).
soil_sf <- st_as_sf(soil_data, coords = c("lon", "lat"), crs = 4326, remove = FALSE)

# Decision: Geostatistical calculations require distances in meters, not degrees.
# We will project the data to a suitable projected CRS for Sri Lanka (UTM Zone 44N).
soil_sf_utm <- st_transform(soil_sf, 32644)

# Add the new projected coordinates back to the main data frame for use in models.
coords_utm <- st_coordinates(soil_sf_utm)
soil_data$x_utm <- coords_utm[, 1]
soil_data$y_utm <- coords_utm[, 2]

# Visualization: Create a map of sample locations.
sample_map <- ggplot(soil_sf_utm) +
  geom_sf(aes(color = dpth), size = 3) +
  scale_color_viridis(name = "Depth (cm)") +
  labs(title = "Soil Depth Sample Locations in Sri Lanka", x = "Easting (m)", y = "Northing (m)") +
  theme_minimal(base_size = 12)

ggsave("out/A2_Sample_Location_Map.png", sample_map, width = 8, height = 10, dpi = 300, bg="white")
cat("Sample location map saved to 'out/A2_Sample_Location_Map.png'.\n")

# --- 6. Save Processed Data for Next Script ---
cat("--- 6. Saving processed data for the next script ---\n")
saveRDS(soil_data, "out/soil_data_processed.rds")

cat("\n=== SCRIPT 01 COMPLETE: Setup and Data Preparation finished. ===\n")

