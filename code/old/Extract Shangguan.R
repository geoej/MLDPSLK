# =============================================================================
# Extract Shangguan Depth to Bedrock Data
# =============================================================================
# 
# Purpose: Extract depth to bedrock (DTB) values from Shangguan et al. global 
#          dataset using point coordinates from existing sampling locations
#
# Input files:
#   - ./external data/BDRICM_M_1km_ll.tif: Global DTB raster (1km resolution)
#   - ./external data/joind.shp: Point locations for value extraction
#
# Output:
#   - ./external data/shangguan.shp: Shapefile with extracted DTB values
#
# Data source: Shangguan et al. global depth to bedrock dataset
# URL: http://globalchange.bnu.edu.cn/research/dtb.jsp
#
# Author: [Your name]
# Date: [Current date]
# =============================================================================

# Load required libraries
# -----------------------
library(raster)      # For raster data handling and extraction
library(sf)          # For shapefile reading/writing and spatial operations
library(sp)          # For spatial data structures (if needed for compatibility)
#library(rgdal)       # For spatial data I/O (if needed)

cat("Loading required libraries...\n")

# Set working directory and file paths
# -----------------------------------
cat("Setting up file paths...\n")

# Input files
raster_file_ricm <- "./external data/BDRICM_M_250m_ll.tif"
raster_file_ticm <- "./external data/BDTICM_M_250m_ll.tif"
points_file <- "./external data/joind.shp"

# Output file
output_file <- "./external data/shangguan.shp"

# Check if input files exist
# -------------------------
if (!file.exists(raster_file_ricm)) {
  stop("Error: RICM raster file not found at ", raster_file_ricm)
}

if (!file.exists(raster_file_ticm)) {
  stop("Error: TICM raster file not found at ", raster_file_ticm)
}

if (!file.exists(points_file)) {
  stop("Error: Shapefile not found at ", points_file)
}

cat("Input files verified successfully.\n")

# Load the depth to bedrock raster
# --------------------------------
cat("Loading Shangguan depth to bedrock raster...\n")

# Load the raster file
ricm_raster <- raster(raster_file_ricm)
ticm_raster <- raster(raster_file_ticm)

# Display raster information
cat("RICM Raster properties:\n")
cat("  - Extent:", as.character(extent(ricm_raster)), "\n")
cat("  - Resolution:", res(ricm_raster), "\n")
cat("  - CRS:", as.character(crs(ricm_raster)), "\n")

cat("TICM Raster properties:\n")
cat("  - Extent:", as.character(extent(ticm_raster)), "\n")
cat("  - Resolution:", res(ticm_raster), "\n")
cat("  - CRS:", as.character(crs(ticm_raster)), "\n")

# Load the point locations shapefile
# ---------------------------------
cat("Loading point locations from shapefile...\n")

# Read the shapefile using sf package
points_sf <- st_read(points_file, quiet = TRUE)

# Display basic information about the points
cat("Point data properties:\n")
cat("  - Number of points:", nrow(points_sf), "\n")
cat("  - CRS:", st_crs(points_sf)$input, "\n")
cat("  - Column names:", paste(names(points_sf), collapse = ", "), "\n")

# Check and transform coordinate systems if necessary
# --------------------------------------------------
cat("Checking coordinate reference systems...\n")

# Get CRS of both datasets
ricm_crs <- crs(ricm_raster)
ticm_crs <- crs(ticm_raster)
points_crs <- st_crs(points_sf)

# Transform points to match raster CRS if they differ
if (!identical(st_crs(points_sf), st_crs(ricm_raster)) | !identical(st_crs(points_sf), st_crs(ticm_raster))){
  cat("Transforming point coordinates to match raster CRS...\n")
  points_sf <- st_transform(points_sf, crs = crs(ricm_raster))
} else {
  cat("Coordinate systems match - no transformation needed.\n")
}

# Extract depth to bedrock values at point locations
# --------------------------------------------------
cat("Extracting depth to bedrock values...\n")

# Convert sf object to SpatialPointsDataFrame for raster extraction
# (raster package works better with sp objects)
points_sp <- as(points_sf, "Spatial")

# Extract raster values at point locations
# Use raster:: prefix to explicitly call raster package's extract function
# This avoids conflicts with tidyr::extract or dplyr::extract
ricm_values <- raster::extract(ricm_raster, points_sp)
ticm_values <- raster::extract(ticm_raster, points_sp)

cat("Extraction completed for", length(ricm_values), "points.\n")

# Check extraction results
# -----------------------
cat("RICM Extraction summary:\n")
cat("  - Valid extractions:", sum(!is.na(ricm_values)), "\n")
cat("  - Missing values (NA):", sum(is.na(ricm_values)), "\n")
if(sum(!is.na(ricm_values)) > 0) {
  cat("  - RICM DTB range:", min(ricm_values, na.rm=TRUE), "to", 
      max(ricm_values, na.rm=TRUE), "cm\n")
}

cat("TICM Extraction summary:\n")
cat("  - Valid extractions:", sum(!is.na(ticm_values)), "\n")
cat("  - Missing values (NA):", sum(is.na(ticm_values)), "\n")
if(sum(!is.na(ticm_values)) > 0) {
  cat("  - TICM DTB range:", min(ticm_values, na.rm=TRUE), "to", 
      max(ticm_values, na.rm=TRUE), "cm\n")
}

# Add extracted values to the points dataset
# ------------------------------------------
cat("Adding extracted values to point dataset...\n")

# Add the Shangguan DTB values as new columns
# Note: Shapefile field names are limited to 10 characters, so we use shorter names
points_sf$ricm_values <- ricm_values
points_sf$ticm_values <- ticm_values

# Optional: Convert cm to m for easier interpretation
#points_sf$shanm <- extracted_values / 100

# Add metadata columns for traceability (with short field names)
#points_sf$extr_date <- as.character(Sys.Date())
#points_sf$data_src <- "Shangguan"
#points_sf$resolution <- "1km"

# Display final dataset information
cat("Final dataset properties:\n")
cat("  - Total columns:", ncol(points_sf), "\n")
cat("  - New columns added: ricm_values, ticm_values\n")

# Save the results as a new shapefile
# -----------------------------------
cat("Saving results to shapefile...\n")

# Create external data directory if it doesn't exist
# output_dir <- dirname(output_file)
#if (!dir.exists(output_dir)) {
#  dir.create(output_dir, recursive = TRUE)
#  cat("Created directory:", output_dir, "\n")
#}

# Write the shapefile with extracted values
# Use append = FALSE and delete_layer = TRUE to overwrite existing files
st_write(points_sf, output_file, append = FALSE, delete_layer = TRUE, quiet = TRUE)

cat("Shapefile saved successfully as:", output_file, "\n")

# Generate summary statistics
# ---------------------------
cat("\n=== EXTRACTION SUMMARY ===\n")
cat("Input RICM raster file:", raster_file_ricm, "\n")
cat("Input TICM raster file:", raster_file_ticm, "\n")
cat("Input points file:", points_file, "\n")
cat("Output shapefile:", output_file, "\n")
cat("Number of points processed:", nrow(points_sf), "\n")
cat("Successful RICM extractions:", sum(!is.na(ricm_values)), "\n")
cat("Failed RICM extractions (NA):", sum(is.na(ricm_values)), "\n")
cat("Successful TICM extractions:", sum(!is.na(ticm_values)), "\n")
cat("Failed TICM extractions (NA):", sum(is.na(ticm_values)), "\n")

if (sum(!is.na(ricm_values)) > 0) {
  cat("RICM DTB statistics (cm):\n")
  cat("  - Minimum:", min(ricm_values, na.rm=TRUE), "\n")
  cat("  - Maximum:", max(ricm_values, na.rm=TRUE), "\n")
  cat("  - Mean:", round(mean(ricm_values, na.rm=TRUE), 2), "\n")
  cat("  - Median:", round(median(ricm_values, na.rm=TRUE), 2), "\n")
  cat("  - Standard deviation:", round(sd(ricm_values, na.rm=TRUE), 2), "\n")
}

if (sum(!is.na(ticm_values)) > 0) {
  cat("TICM DTB statistics (cm):\n")
  cat("  - Minimum:", min(ticm_values, na.rm=TRUE), "\n")
  cat("  - Maximum:", max(ticm_values, na.rm=TRUE), "\n")
  cat("  - Mean:", round(mean(ticm_values, na.rm=TRUE), 2), "\n")
  cat("  - Median:", round(median(ticm_values, na.rm=TRUE), 2), "\n")
  cat("  - Standard deviation:", round(sd(ticm_values, na.rm=TRUE), 2), "\n")
}

# Create comprehensive comparison plots
# -------------------------------------
if (require(ggplot2, quietly = TRUE) && require(dplyr, quietly = TRUE)) {
  cat("\nCreating comparison plots...\n")
  
  # Prepare data for plotting - convert cm to m for easier comparison
  plot_data <- data.frame(
    Real_Depth = points_sf$`Depth..m.`,
    RICM_Censored = ricm_values, #/ 100,  # Convert cm to m
    TICM_Absolute = ticm_values #/ 100   # Convert cm to m
  ) %>%
    filter(!is.na(Real_Depth) & !is.na(RICM_Censored) & !is.na(TICM_Absolute))
  
  cat("Number of complete cases for plotting:", nrow(plot_data), "\n")
  
  # 1. Scatter plot: Real vs RICM (Censored)
  p1 <- ggplot(plot_data, aes(x = Real_Depth, y = RICM_Censored)) +
    geom_point(alpha = 0.6, color = "steelblue", size = 2) +
    geom_smooth(method = "lm", color = "red", se = TRUE) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
    labs(title = "Real Depth vs Censored DTB (RICM)",
         subtitle = paste0("n = ", nrow(plot_data), " points"),
         x = "Measured Depth (m)",
         y = "RICM Censored DTB (m)") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5)) +
    coord_equal(xlim = c(0, max(c(plot_data$Real_Depth, plot_data$RICM_Censored), na.rm = TRUE)),
                ylim = c(0, max(c(plot_data$Real_Depth, plot_data$RICM_Censored), na.rm = TRUE)))
  
  # 2. Scatter plot: Real vs TICM (Absolute)
  p2 <- ggplot(plot_data, aes(x = Real_Depth, y = TICM_Absolute)) +
    geom_point(alpha = 0.6, color = "darkgreen", size = 2) +
    geom_smooth(method = "lm", color = "red", se = TRUE) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
    labs(title = "Real Depth vs Absolute DTB (TICM)",
         subtitle = paste0("n = ", nrow(plot_data), " points"),
         x = "Measured Depth (m)",
         y = "TICM Absolute DTB (m)") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5)) +
    coord_equal(xlim = c(0, max(c(plot_data$Real_Depth, plot_data$RICM_Censored), na.rm = TRUE)),
                ylim = c(0, max(c(plot_data$Real_Depth, plot_data$TICM_Absolute), na.rm = TRUE)))
  
  # 3. Comparison of both extractions vs real data
  library(tidyr)
  plot_data_long <- plot_data %>%
    pivot_longer(cols = c(RICM_Censored, TICM_Absolute),
                names_to = "DTB_Type", 
                values_to = "DTB_Value")
  
  p3 <- ggplot(plot_data_long, aes(x = Real_Depth, y = DTB_Value, color = DTB_Type)) +
    geom_point(alpha = 0.6, size = 2) +
    geom_smooth(method = "lm", se = TRUE) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
    scale_color_manual(values = c("RICM_Censored" = "steelblue", "TICM_Absolute" = "darkgreen"),
                      labels = c("RICM (Censored)", "TICM (Absolute)")) +
    labs(title = "Real Depth vs Both DTB Extractions",
         subtitle = "Comparison of Absolute and Censored DTB",
         x = "Measured Depth (m)",
         y = "Extracted DTB (m)",
         color = "DTB Type") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          legend.position = "bottom")
  
  # 4. Histogram comparison
  p4 <- ggplot(plot_data_long, aes(x = DTB_Value, fill = DTB_Type)) +
    geom_histogram(alpha = 0.7, position = "identity", bins = 20) +
    geom_vline(aes(xintercept = mean(Real_Depth, na.rm = TRUE)), 
               linetype = "dashed", color = "red", size = 1) +
    scale_fill_manual(values = c("RICM_Censored" = "steelblue", "TICM_Absolute" = "darkgreen"),
                     labels = c("RICM (Censored)", "TICM (Absolute)")) +
    labs(title = "Distribution of DTB Extractions vs Real Depth",
         subtitle = "Red line shows mean of real depth measurements",
         x = "Depth (m)",
         y = "Frequency",
         fill = "Data Type") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          legend.position = "bottom")
  
  # Calculate correlation coefficients
  cor_ricm <- cor(plot_data$Real_Depth, plot_data$RICM_Censored, use = "complete.obs")
  cor_ticm <- cor(plot_data$Real_Depth, plot_data$TICM_Absolute, use = "complete.obs")
  
  cat("Correlation coefficients:\n")
  cat("Real Depth vs RICM (Censored):", round(cor_ricm, 3), "\n")
  cat("Real Depth vs TICM (Absolute):", round(cor_ticm, 3), "\n")
  
  # Save plots
  if (!dir.exists("./out")) {
    dir.create("./out", recursive = TRUE)
  }
  
  ggsave("./out/real_vs_ricm_scatter.png", p1, width = 10, height = 8, dpi = 300)
  ggsave("./out/real_vs_ticm_scatter.png", p2, width = 10, height = 8, dpi = 300)
  ggsave("./out/dtb_comparison_both.png", p3, width = 10, height = 8, dpi = 300)
  ggsave("./out/dtb_histogram_comparison.png", p4, width = 10, height = 6, dpi = 300)
  
  cat("Plots saved:\n")
  cat("  - ./out/real_vs_ricm_scatter.png\n")
  cat("  - ./out/real_vs_ticm_scatter.png\n")
  cat("  - ./out/dtb_comparison_both.png\n")
  cat("  - ./out/dtb_histogram_comparison.png\n")
  
  # Create a summary statistics table
  summary_stats <- data.frame(
    Dataset = c("Real Depth", "RICM (Censored)", "TICM (Absolute)"),
    Mean = c(mean(plot_data$Real_Depth, na.rm = TRUE),
            mean(plot_data$RICM_Censored, na.rm = TRUE),
            mean(plot_data$TICM_Absolute, na.rm = TRUE)),
    Median = c(median(plot_data$Real_Depth, na.rm = TRUE),
              median(plot_data$RICM_Censored, na.rm = TRUE),
              median(plot_data$TICM_Absolute, na.rm = TRUE)),
    SD = c(sd(plot_data$Real_Depth, na.rm = TRUE),
          sd(plot_data$RICM_Censored, na.rm = TRUE),
          sd(plot_data$TICM_Absolute, na.rm = TRUE)),
    Min = c(min(plot_data$Real_Depth, na.rm = TRUE),
           min(plot_data$RICM_Censored, na.rm = TRUE),
           min(plot_data$TICM_Absolute, na.rm = TRUE)),
    Max = c(max(plot_data$Real_Depth, na.rm = TRUE),
           max(plot_data$RICM_Censored, na.rm = TRUE),
           max(plot_data$TICM_Absolute, na.rm = TRUE))
  )
  
  write.csv(summary_stats, "./out/dtb_comparison_summary.csv", row.names = FALSE)
  cat("  - ./out/dtb_comparison_summary.csv\n")
  
  print(summary_stats)
}

cat("\n=== PROCESS COMPLETED SUCCESSFULLY ===\n")
cat("The extracted Shangguan DTB values are now available in:", output_file, "\n")
cat("You can use these values for comparison with your measured soil depth data.\n")

# Clean up workspace (optional)
# -----------------------------
# Remove large objects to free memory
rm(ricm_raster, ticm_raster)
cat("Cleanup completed.\n")