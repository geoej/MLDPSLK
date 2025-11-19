################################################################################
# SOIL DEPTH ANALYSIS - SCRIPT 12
# SAMPLE REPRESENTATIVENESS ANALYSIS
#
# Purpose:
# 1. Analyze spatial distribution of samples
# 2. Compare sample distribution across elevation ranges vs. national distribution
# 3. Calculate nearest neighbor distances and clustering indices
# 4. Assess sampling density across different regions
# 5. Identify under-sampled regions and implications
#
# Output:
# - Sampling density map
# - Elevation distribution comparison
# - Nearest neighbor distance analysis
# - Clustering indices
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
    library(sf)
    library(terra)
    library(ggplot2)
    library(viridis)
    library(cowplot)
    library(tidyterra)
})

# --- 2. Load Data ---
cat("--- 2. Loading data ---\n")
soil_data <- readRDS("out/soil_data_processed.rds")
soil_sf <- st_as_sf(soil_data, coords = c("lon", "lat"), crs = 4326)

# Load elevation raster for comparison
covariates_dir <- "/Users/ej/CFFRC/04-Research/Soil/SLK soil data/Covariates/ISRIC covariates /isric_gsp/Covs"
elevation_raster <- rast(file.path(covariates_dir, "DEMENV5.tif"))

# --- 3. Nearest Neighbor Distance Analysis ---
cat("--- 3. Calculating nearest neighbor distances ---\n")

# Convert to UTM for distance calculations
soil_utm <- st_transform(soil_sf, crs = 32644)  # UTM Zone 44N

# Calculate pairwise distances
coords <- st_coordinates(soil_utm)
dist_matrix <- as.matrix(dist(coords))

# Set diagonal to NA (distance to self)
diag(dist_matrix) <- NA

# Calculate nearest neighbor distances
nn_distances <- apply(dist_matrix, 1, function(x) min(x, na.rm = TRUE))

# Convert to km
nn_distances_km <- nn_distances / 1000

# Summary statistics
nn_summary <- data.frame(
    Metric = c("Mean", "Median", "Min", "Max", "SD"),
    Distance_km = c(
        mean(nn_distances_km),
        median(nn_distances_km),
        min(nn_distances_km),
        max(nn_distances_km),
        sd(nn_distances_km)
    )
)

cat("\nNearest Neighbor Distance Summary:\n")
print(nn_summary)

# Save results
write_csv(data.frame(Sample_ID = 1:length(nn_distances_km), 
                     Nearest_Neighbor_Distance_km = nn_distances_km),
          "out/sample_nearest_neighbor_distances.csv")

# --- 4. Clustering Analysis ---
cat("--- 4. Performing clustering analysis ---\n")

# Calculate Clark-Evans index (R statistic)
# R = observed mean NN distance / expected mean NN distance under CSR
# R < 1 indicates clustering, R > 1 indicates regularity, R ≈ 1 indicates random
coords_utm <- st_coordinates(soil_utm)
n_points <- nrow(coords_utm)

# Calculate bounding box area
bbox <- st_bbox(soil_utm)
area_m2 <- (bbox["xmax"] - bbox["xmin"]) * (bbox["ymax"] - bbox["ymin"])
area_km2 <- area_m2 / 1e6  # Convert m² to km²

# Expected mean NN distance under Complete Spatial Randomness (CSR)
# E[d] = 1 / (2 * sqrt(λ)) where λ is the intensity (points per unit area)
intensity <- n_points / area_km2  # points per km²
expected_nn_km <- 1 / (2 * sqrt(intensity))  # Expected NN distance in km under CSR

observed_nn_km <- mean(nn_distances_km)
clark_evans_R <- observed_nn_km / expected_nn_km

cat(paste("\nClark-Evans Index (R):", round(clark_evans_R, 3), "\n"))
cat(paste("  R < 1 indicates clustering\n"))
cat(paste("  R ≈ 1 indicates random distribution\n"))
cat(paste("  R > 1 indicates regular distribution\n"))
cat(paste("  Observed mean NN:", round(observed_nn_km, 2), "km\n"))
cat(paste("  Expected mean NN (CSR):", round(expected_nn_km, 2), "km\n"))

# Save clustering results
clustering_results <- data.frame(
    Metric = c("Clark-Evans_R", "Observed_Mean_NN_km", "Expected_Mean_NN_km_CSR", 
               "Area_km2", "Sample_Density_per_km2", "Intensity_points_per_km2"),
    Value = c(clark_evans_R, observed_nn_km, expected_nn_km, area_km2, 
              n_points / area_km2, intensity)
)
write_csv(clustering_results, "out/sample_clustering_analysis.csv")

# --- 5. Elevation Distribution Comparison ---
cat("--- 5. Comparing sample elevation distribution with national distribution ---\n")

# Extract elevation at sample points
sample_elevations <- terra::extract(elevation_raster, vect(soil_sf))[,2]

# Get national elevation distribution (sample from raster)
# Sample 10,000 random points for national distribution
set.seed(42)
national_elevations <- terra::spatSample(elevation_raster, size = 10000, na.rm = TRUE)[,1]

# Create elevation bins
elevation_bins <- seq(0, 2500, by = 100)
sample_elev_hist <- hist(sample_elevations, breaks = elevation_bins, plot = FALSE)
national_elev_hist <- hist(national_elevations, breaks = elevation_bins, plot = FALSE)

# Calculate proportions
sample_prop <- sample_elev_hist$counts / sum(sample_elev_hist$counts)
national_prop <- national_elev_hist$counts / sum(national_elev_hist$counts)

# Create comparison data frame
elevation_comparison <- data.frame(
    Elevation_Band_m = elevation_bins[-length(elevation_bins)] + 50,  # Midpoint
    Sample_Proportion = sample_prop,
    National_Proportion = national_prop,
    Sample_Count = sample_elev_hist$counts,
    National_Count = national_elev_hist$counts
) %>%
    filter(Sample_Count > 0 | National_Proportion > 0.01)  # Filter to relevant bands

write_csv(elevation_comparison, "out/sample_elevation_distribution_comparison.csv")

cat("\nElevation Distribution Summary:\n")
cat(paste("Sample elevation range:", round(min(sample_elevations, na.rm = TRUE)), "-", 
          round(max(sample_elevations, na.rm = TRUE)), "m\n"))
cat(paste("Sample mean elevation:", round(mean(sample_elevations, na.rm = TRUE)), "m\n"))
cat(paste("National elevation range:", round(min(national_elevations, na.rm = TRUE)), "-", 
          round(max(national_elevations, na.rm = TRUE)), "m\n"))
cat(paste("National mean elevation:", round(mean(national_elevations, na.rm = TRUE)), "m\n"))

# --- 6. Sampling Density Analysis ---
cat("--- 6. Analyzing sampling density ---\n")

# Calculate sampling density (samples per 1000 km²)
sri_lanka_area_km2 <- 65610  # Known area of Sri Lanka
overall_density <- (nrow(soil_data) / sri_lanka_area_km2) * 1000

cat(paste("\nOverall sampling density:", round(overall_density, 2), "samples per 1000 km²\n"))

# Create a grid for density calculation
# Use 50 km grid cells
grid_size_km <- 50
bbox <- st_bbox(soil_utm)
x_range <- bbox[c("xmin", "xmax")]
y_range <- bbox[c("ymin", "ymax")]

x_seq <- seq(x_range[1], x_range[2], by = grid_size_km * 1000)
y_seq <- seq(y_range[1], y_range[2], by = grid_size_km * 1000)

# Create grid and count points in each cell
grid_list <- list()
for (i in 1:(length(x_seq)-1)) {
    for (j in 1:(length(y_seq)-1)) {
        cell <- st_polygon(list(rbind(
            c(x_seq[i], y_seq[j]),
            c(x_seq[i+1], y_seq[j]),
            c(x_seq[i+1], y_seq[j+1]),
            c(x_seq[i], y_seq[j+1]),
            c(x_seq[i], y_seq[j])
        )))
        cell_sf <- st_sf(geometry = st_sfc(cell, crs = 32644))
        n_points <- sum(st_intersects(soil_utm, cell_sf, sparse = FALSE))
        if (n_points > 0 || TRUE) {  # Include all cells for visualization
            grid_list[[length(grid_list) + 1]] <- data.frame(
                x = x_seq[i] + grid_size_km * 500,
                y = y_seq[j] + grid_size_km * 500,
                n_samples = n_points,
                density_per_1000km2 = (n_points / (grid_size_km^2)) * 1000
            )
        }
    }
}

density_grid <- bind_rows(grid_list)
write_csv(density_grid, "out/sampling_density_grid.csv")

# --- 7. Create Visualizations ---
cat("--- 7. Creating visualizations ---\n")

# 7.1 Nearest Neighbor Distance Distribution
nn_plot <- ggplot(data.frame(Distance_km = nn_distances_km), aes(x = Distance_km)) +
    geom_histogram(bins = 20, fill = "steelblue", alpha = 0.7, color = "black") +
    geom_vline(xintercept = mean(nn_distances_km), color = "red", linetype = "dashed", linewidth = 1) +
    geom_vline(xintercept = expected_nn_km, color = "darkgreen", linetype = "dashed", linewidth = 1) +
    annotate("text", x = mean(nn_distances_km), y = Inf, label = paste("Observed Mean =", round(mean(nn_distances_km), 2), "km"), 
             vjust = 1.5, hjust = -0.1, color = "red", fontface = "bold") +
    annotate("text", x = expected_nn_km, y = Inf, label = paste("Expected (CSR) =", round(expected_nn_km, 2), "km"), 
             vjust = 2.5, hjust = -0.1, color = "darkgreen", fontface = "bold") +
    labs(title = "Nearest Neighbor Distance Distribution",
         subtitle = paste("Clark-Evans Index R =", round(clark_evans_R, 3), 
                         ifelse(clark_evans_R < 0.8, "(Clustered)", 
                               ifelse(clark_evans_R > 1.2, "(Regular)", "(Random)"))),
         x = "Nearest Neighbor Distance (km)",
         y = "Frequency") +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold"))

ggsave("out/Figure_Sample_NN_Distance_Distribution.png", nn_plot, 
       width = 8, height = 6, dpi = 300, bg = "white")

# 7.2 Elevation Distribution Comparison
elevation_comparison_long <- elevation_comparison %>%
    tidyr::pivot_longer(cols = c(Sample_Proportion, National_Proportion),
                       names_to = "Source", values_to = "Proportion") %>%
    mutate(Source = ifelse(Source == "Sample_Proportion", "Sample Distribution", "National Distribution"))

elevation_plot <- ggplot(elevation_comparison_long, 
                        aes(x = Elevation_Band_m, y = Proportion, fill = Source)) +
    geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
    scale_fill_manual(values = c("Sample Distribution" = "steelblue", 
                                 "National Distribution" = "darkgreen")) +
    labs(title = "Elevation Distribution Comparison",
         subtitle = "Sample distribution vs. national distribution",
         x = "Elevation (m)",
         y = "Proportion",
         fill = "Source") +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold"),
          legend.position = "bottom")

ggsave("out/Figure_Sample_Elevation_Distribution.png", elevation_plot, 
       width = 10, height = 6, dpi = 300, bg = "white")

# 7.3 Sampling Density Map
# Create a simple map showing sample locations with density
# Load final map for background
final_map <- rast("out/Final_Ensemble_Soil_Depth_Map.tif")
final_map_wgs84 <- project(final_map, "EPSG:4326")

sampling_map <- ggplot() +
    geom_spatraster(data = final_map_wgs84, alpha = 0.3) +
    scale_fill_viridis_c(name = "Depth (cm)", na.value = "transparent", guide = "none") +
    geom_sf(data = soil_sf, color = "red", fill = "white", shape = 21, size = 3, stroke = 1.5) +
    labs(title = "Sample Locations and Spatial Distribution",
         subtitle = paste("n =", nrow(soil_data), "samples; Mean NN distance =", 
                         round(mean(nn_distances_km), 2), "km"),
         x = "Longitude", y = "Latitude") +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold")) +
    coord_sf(expand = FALSE)

ggsave("out/Figure_Sample_Spatial_Distribution.png", sampling_map, 
       width = 8, height = 10, dpi = 300, bg = "white")

# --- 8. Summary Statistics ---
cat("--- 8. Generating summary statistics ---\n")

summary_stats <- data.frame(
    Metric = c("Total_Samples", "Mean_NN_Distance_km", "Median_NN_Distance_km", 
               "Min_NN_Distance_km", "Max_NN_Distance_km", "SD_NN_Distance_km",
               "Clark_Evans_R", "Sampling_Density_per_1000km2",
               "Sample_Mean_Elevation_m", "National_Mean_Elevation_m",
               "Sample_Elevation_Range_m", "National_Elevation_Range_m"),
    Value = c(
        nrow(soil_data),
        round(mean(nn_distances_km), 2),
        round(median(nn_distances_km), 2),
        round(min(nn_distances_km), 2),
        round(max(nn_distances_km), 2),
        round(sd(nn_distances_km), 2),
        round(clark_evans_R, 3),
        round(overall_density, 2),
        round(mean(sample_elevations, na.rm = TRUE), 0),
        round(mean(national_elevations, na.rm = TRUE), 0),
        paste(round(min(sample_elevations, na.rm = TRUE), 0), "-", 
              round(max(sample_elevations, na.rm = TRUE), 0)),
        paste(round(min(national_elevations, na.rm = TRUE), 0), "-", 
              round(max(national_elevations, na.rm = TRUE), 0))
    )
)

write_csv(summary_stats, "out/sample_representativeness_summary.csv")

cat("\n=== SAMPLE REPRESENTATIVENESS SUMMARY ===\n")
print(summary_stats)

cat("\n=== SCRIPT 12 COMPLETE: Sample representativeness analysis finished. ===\n")

