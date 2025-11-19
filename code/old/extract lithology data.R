# =============================================================================
# Extract Global Lithological Map (GLiM) data
# =============================================================================
# 
# Purpose: Extract lithology data at the location of 'real' soil data
#
# Input files: "./external data/joind.shp"
#   - /Users/ej/CFFRC/04-Research/Soil/SLK soil data/Covariates/Global litholoigical map hartmann-moosdorf_2012/glim_wgs84_0point5deg.txt.asc
#   
#
# Output:
#   - ./external data/glim_0point5deg.shp: Shapefile with lithology data
#   - ./external data/glim_0point5deg.csv: CSV with lithology data
#
# Data source: Global Lithological Map database (GLiM)
# URL: https://doi.pangaea.de/10.1594/PANGAEA.788537
# Reference: Hartmann & Moosdorf (2012)
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
lithology_file <- "/Users/ej/CFFRC/04-Research/Soil/SLK soil data/Covariates/Global litholoigical map hartmann-moosdorf_2012/glim_wgs84_0point5deg.txt.asc"

# Output file
output_file <- "./external data/glim_0point5deg.shp"
output_csv <- "./external data/glim_0point5deg.csv"

# Create lithology class lookup table
# ------------------------------------
cat("Creating lithology class lookup table...\n")
lithology_classes <- data.frame(
  Value = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16),
  Code = c("su", "vb", "ss", "pb", "sm", "sc", "va", "mt", "pa", "vi", "wb", "py", "pi", "ev", "nd", "ig"),
  Description = c(
    "Unconsolidated sediments",
    "Basic volcanic rocks",
    "Siliciclastic sedimentary rocks",
    "Basic plutonic rocks",
    "Mixed sedimentary rocks",
    "Carbonate sedimentary rocks",
    "Acid volcanic rocks",
    "Metamorphics",
    "Acid plutonic rocks",
    "Intermediate volcanic rocks",
    "Water bodies",
    "Pyroclastics",
    "Intermediate plutonic rocks",
    "Evaporites",
    "No data",
    "Ice and glaciers"
  ),
  stringsAsFactors = FALSE
)

cat("Lithology classes defined:\n")
print(lithology_classes)

# Read the soil data points
# -------------------------
cat("\nReading soil data points...\n")
soil_points <- st_read(points_file)
cat(paste("Loaded", nrow(soil_points), "soil data points\n"))

# Read the lithology raster
# -------------------------
cat("Reading lithology raster...\n")
lithology_raster <- raster(lithology_file)
cat("Lithology raster loaded successfully\n")
cat(paste("Raster dimensions:", ncol(lithology_raster), "x", nrow(lithology_raster), "\n"))
cat(paste("Raster CRS:", crs(lithology_raster), "\n"))

# Extract lithology values at point locations
# --------------------------------------------
cat("Extracting lithology values...\n")
lithology_values <- extract(lithology_raster, soil_points)
cat(paste("Extracted", length(lithology_values), "lithology values\n"))

# Create one-hot encoded lithology columns
# ------------------------------------------
cat("Creating one-hot encoded lithology columns...\n")

# Initialize one-hot matrix with zeros
lithology_onehot <- matrix(0, nrow = length(lithology_values), ncol = nrow(lithology_classes))
colnames(lithology_onehot) <- paste0("Litho_", lithology_classes$Code)

# Fill in the one-hot encoding
for (i in seq_along(lithology_values)) {
  if (!is.na(lithology_values[i])) {
    # Find which class this value corresponds to
    class_index <- which(lithology_classes$Value == lithology_values[i])
    if (length(class_index) > 0) {
      lithology_onehot[i, class_index] <- 1
    }
  }
}

# Convert to data frame
lithology_data <- data.frame(
  Lithology_Value = lithology_values,
  lithology_onehot,
  stringsAsFactors = FALSE
)

# Handle any NA values
na_count <- sum(is.na(lithology_values))
if (na_count > 0) {
  cat(paste("Warning:", na_count, "points have no lithology data (likely water/no data areas)\n"))
}

# Show summary of one-hot encoding
cat("One-hot encoded columns created:\n")
for (i in 1:nrow(lithology_classes)) {
  code <- lithology_classes$Code[i]
  col_name <- paste0("Litho_", code)
  count <- sum(lithology_data[, col_name], na.rm = TRUE)
  desc <- lithology_classes$Description[i]
  cat(paste(col_name, ":", count, "points (", desc, ")\n"))
}

# Combine original data with lithology information
# ------------------------------------------------
cat("Combining data...\n")

# Convert soil points to regular data frame (without geometry)
soil_data_df <- st_drop_geometry(soil_points)

# Combine with lithology data
combined_df <- cbind(soil_data_df, lithology_data)

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

# Summary statistics
# ------------------
cat("\n=== LITHOLOGY EXTRACTION COMPLETE ===\n")
cat(paste("Total soil points processed:", nrow(soil_points), "\n"))
cat(paste("Points with lithology data:", sum(!is.na(lithology_values)), "\n"))
cat(paste("Points without lithology data:", sum(is.na(lithology_values)), "\n"))

# Show frequency of each lithology class from one-hot encoding
cat("\nLithology class frequency (from one-hot encoding):\n")
for (i in 1:nrow(lithology_classes)) {
  code <- lithology_classes$Code[i]
  desc <- lithology_classes$Description[i]
  col_name <- paste0("Litho_", code)
  count <- sum(lithology_data[, col_name], na.rm = TRUE)
  cat(paste(code, "(", desc, "):", count, "\n"))
}

cat("\nScript completed successfully!\n")
