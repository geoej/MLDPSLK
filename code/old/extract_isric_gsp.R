# =============================================================================
# Extract ISRIC GSP covariates
# =============================================================================
# 
# Purpose: Extract coordinates at the location of 'real' soil data
#
# Input files: /Users/ej/CFFRC/04-Research/Soil/SLK soil data/Covariates/ISRIC covariates /isric_gsp/Covs
#   - 
#   - 
#
# Output:
#   - ./external data/isric_gsp.shp: Shapefile with real data
#
# Data source: GSP and ISRIC environmental covariates data repository
# URL: https://files.isric.org/projects/gsp/Sri_Lanka/
#
# Author: Ebrahim Jahanshiri
# Date: 
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
points_file <- "./external data/joind.shp"
covariates_dir <- "/Users/ej/CFFRC/04-Research/Soil/SLK soil data/Covariates/ISRIC covariates /isric_gsp/Covs"

# Output file
output_file <- "./external data/isric_gsp.shp"
output_csv <- "./external data/isric_gsp.csv"

# Read the soil data points
# -------------------------
cat("Reading soil data points...\n")
soil_points <- st_read(points_file)
cat(paste("Loaded", nrow(soil_points), "soil data points\n"))

# Get list of all TIF files in covariates directory
# --------------------------------------------------
cat("Getting list of covariate files...\n")
tif_files <- list.files(covariates_dir, pattern = "\\.tif$", full.names = TRUE)
cat(paste("Found", length(tif_files), "covariate files\n"))

# Extract just the file names without path and extension for column names
covariate_names <- tools::file_path_sans_ext(basename(tif_files))

# Initialize progress tracking
cat("Starting covariate extraction...\n")
total_files <- length(tif_files)

# Create a list to store extracted values for each covariate
extracted_values <- list()

# Extract values for each covariate
# ----------------------------------
for (i in seq_along(tif_files)) {
  tif_file <- tif_files[i]
  covariate_name <- covariate_names[i]
  
  cat(paste("Processing", covariate_name, "(", i, "of", total_files, ")...\n"))
  
  # Read the raster
  raster_layer <- raster(tif_file)
  
  # Extract values at point locations
  # Note: extract() returns values in the same order as the input points
  extracted_vals <- extract(raster_layer, soil_points)
  
  # Store the extracted values
  extracted_values[[covariate_name]] <- extracted_vals
  
  # Clean up memory
  rm(raster_layer)
}

# Combine original data with extracted covariate values
# -----------------------------------------------------
cat("Combining data...\n")

# Convert extracted values list to data frame
covariates_df <- data.frame(extracted_values, stringsAsFactors = FALSE)

# Combine with original soil data
# Note: We need to be careful about spatial attributes
soil_data_df <- st_drop_geometry(soil_points)  # Remove geometry to get regular data frame
combined_df <- cbind(soil_data_df, covariates_df)

# Create spatial object with combined data
combined_sf <- st_sf(combined_df, geometry = st_geometry(soil_points))

# Save results
# ------------
cat("Saving results...\n")

# Save as shapefile
st_write(combined_sf, output_file, delete_dsn = TRUE)
cat(paste("Shapefile saved to:", output_file, "\n"))

# Save as CSV (without geometry)
write.csv(combined_df, output_csv, row.names = FALSE)
cat(paste("CSV saved to:", output_csv, "\n"))

# Summary
# -------
cat("\n=== EXTRACTION COMPLETE ===\n")
cat(paste("Total soil points processed:", nrow(soil_points), "\n"))
cat(paste("Total covariates extracted:", length(tif_files), "\n"))
cat(paste("Output shapefile:", output_file, "\n"))
cat(paste("Output CSV:", output_csv, "\n"))

# Display first few covariate names for verification
cat("\nFirst 10 covariate names:\n")
print(head(covariate_names, 10))

cat("\nScript completed successfully!\n")
