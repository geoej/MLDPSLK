# =============================================================================
# Merge Covariate Shapefiles and Create Final Dataset
# =============================================================================
# 
# Purpose: Merge ISRIC GSP covariates and lithology data into final dataset
#
# Input files: 
#   - "./external data/isric_gsp.shp" - ISRIC covariates data
#   - "./external data/glim_0point5deg.shp" - Lithology one-hot encoded data
#   
# Output:
#   - "./external data/final_covariates.shp": Final shapefile with selected columns
#   - "./external data/final_covariates.csv": Final CSV with selected columns
#
# Author: Ebrahim Jahanshiri
# Date: 
# =============================================================================

# Load required libraries
# -----------------------
library(sf)          # For shapefile reading/writing and spatial operations
library(dplyr)       # For data manipulation

cat("Loading required libraries...\n")

# Set working directory and file paths
# -----------------------------------
cat("Setting up file paths...\n")

# Input files
isric_file <- "./external data/isric_gsp.shp"
lithology_file <- "./external data/glim_0point5deg.shp"

# Output files
output_shp <- "./external data/final_covariates.shp"
output_csv <- "./external data/final_covariates.csv"

# Read both shapefiles
# --------------------
cat("Reading ISRIC covariates shapefile...\n")
isric_data <- st_read(isric_file)
cat(paste("ISRIC data: ", nrow(isric_data), "rows,", ncol(isric_data), "columns\n"))

cat("Reading lithology shapefile...\n")
lithology_data <- st_read(lithology_file)
cat(paste("Lithology data: ", nrow(lithology_data), "rows,", ncol(lithology_data), "columns\n"))

# Check if both datasets have the same number of points and same IDs
# ------------------------------------------------------------------
if (nrow(isric_data) != nrow(lithology_data)) {
  stop("Error: Different number of points in the two datasets")
}

# Verify that the IDs match (assuming both are ordered the same way)
if (!all(isric_data$Id == lithology_data$Id)) {
  stop("Error: IDs do not match between datasets")
}

cat("Data validation passed - both datasets have matching points\n")

# Define column mapping from original names to required final names
# -----------------------------------------------------------------
cat("Mapping column names...\n")

# Create the final column selection and renaming
# Note: We'll take basic info from either file (they should be identical)
# Climate data from isric_data, ISRIC covariates from isric_data, lithology from lithology_data

final_columns <- list(
  # Basic information (from lithology file as it has the abbreviated names we want)
  "Id" = "Id",
  "Latitud" = "Latitud", 
  "Longitd" = "Longitd",
  "Dpth___" = "Dpth___",
  
  # Solar radiation (from isric_data)
  "srad1" = "srad1", "srad2" = "srad2", "srad3" = "srad3", "srad4" = "srad4", 
  "srad5" = "srad5", "srad6" = "srad6", "srad7" = "srad7", "srad8" = "srad8", 
  "srad9" = "srad9", "srad10" = "srad10", "srad11" = "srad11", "srad12" = "srad12",
  
  # Vapor pressure (from isric_data)
  "vapr1" = "vapr1", "vapr2" = "vapr2", "vapr3" = "vapr3", "vapr4" = "vapr4",
  "vapr5" = "vapr5", "vapr6" = "vapr6", "vapr7" = "vapr7", "vapr8" = "vapr8",
  "vapr9" = "vapr9", "vapr10" = "vapr10", "vapr11" = "vapr11", "vapr12" = "vapr12",
  
  # Wind (from isric_data)
  "wind1" = "wind1", "wind2" = "wind2", "wind3" = "wind3", "wind4" = "wind4",
  "wind5" = "wind5", "wind6" = "wind6", "wind7" = "wind7", "wind8" = "wind8",
  "wind9" = "wind9", "wind10" = "wind10", "wind11" = "wind11", "wind12" = "wind12",
  
  # Temperature min (from isric_data)
  "tmin1" = "tmin1", "tmin2" = "tmin2", "tmin3" = "tmin3", "tmin4" = "tmin4",
  "tmin5" = "tmin5", "tmin6" = "tmin6", "tmin7" = "tmin7", "tmin8" = "tmin8",
  "tmin9" = "tmin9", "tmin10" = "tmin10", "tmin11" = "tmin11", "tmin12" = "tmin12",
  
  # Temperature max (from isric_data)
  "tmax1" = "tmax1", "tmax2" = "tmax2", "tmax3" = "tmax3", "tmax4" = "tmax4",
  "tmax5" = "tmax5", "tmax6" = "tmax6", "tmax7" = "tmax7", "tmax8" = "tmax8",
  "tmax9" = "tmax9", "tmax10" = "tmax10", "tmax11" = "tmax11", "tmax12" = "tmax12",
  
  # Elevation (from lithology_data - already abbreviated)
  "elevatn" = "elevatn"
)

# ISRIC covariate columns (from isric_data) - these should match exactly
isric_covariates <- c(
  "B02CHE3", "B04CHE3", "B07CHE3", "B13CHE3", "B14CHE3", "BARL10", "C01MCF5", "C02MCF5",
  "C03MCF5", "C04MCF5", "C05MCF5", "C06MCF5", "C07MCF5", "C08MCF5", "C09MCF5", "C10MCF5",
  "C11MCF5", "C12MCF5", "CHAGSW7", "CRDMRG5", "CRUMRG5", "CRVMRG5", "DEMENV5", "DV2MRG5",
  "DVMMRG5", "ENTENV3", "ES1MOD5", "ES2MOD5", "ES3MOD5", "ES4MOD5", "ES5MOD5", "ES6MOD5",
  "EVEENV3", "EX1MOD5", "EX2MOD5", "EX3MOD5", "EX4MOD5", "EX5MOD5", "EX6MOD5", "EXTGSW7",
  "F01USG5", "F02USG5", "F03USG5", "F04USG5", "F05USG5", "F06USG5", "F07USG5", "I01MOD4",
  "I02MOD4", "I03MOD4", "I04MOD4", "I05MOD4", "I06MOD4", "I07MOD4", "I08MOD4", "I09MOD4",
  "I10MOD4", "I11MOD4", "I12MOD4", "LCEE10", "M01MOD4", "M02MOD4", "M03MOD4", "M04MOD4",
  "M05MOD4", "M06MOD4", "M07MOD4", "M08MOD4", "M09MOD4", "M10MOD4", "M11MOD4", "M12MOD4",
  "MANMCF5", "MAXENV3", "MRNMRG5", "N01MOD3", "N01MSD3", "N02MOD3", "N02MSD3", "N03MOD3",
  "N03MSD3", "N04MOD3", "N04MSD3", "N05MOD3", "N05MSD3", "N06MOD3", "N06MSD3", "N07MOD3",
  "N07MSD3", "N08MOD3", "N08MSD3", "N09MOD3", "N09MSD3", "N10MOD3", "N10MSD3", "N11MOD3",
  "N11MSD3", "N12MOD3", "N12MSD3", "NEGMRG5", "NIRL00", "NIRL14", "OCCGSW7", "P01CHE3",
  "P02CHE3", "P03CHE3", "P04CHE3", "P05CHE3", "P06CHE3", "P07CHE3", "P08CHE3", "P09CHE3",
  "P10CHE3", "P11CHE3", "P12CHE3", "POSMRG5", "PRSCHE3", "QUAUEA3", "RANENV3", "REDL00",
  "REDL14", "S03ESA4", "S04ESA4", "S05ESA4", "S06ESA4", "S07ESA4", "S08ESA4", "S09ESA4",
  "S10ESA4", "SLPMRG5", "SW1L00", "SW1L14", "SW2L00", "SW2L14", "T01MOD3", "T01MSD3",
  "T02MOD3", "T02MSD3", "T03MOD3", "T03MSD3", "T04MOD3", "T04MSD3", "T05MOD3", "T05MSD3",
  "T06MOD3", "T06MSD3", "T07MOD3", "T07MSD3", "T08MOD3", "T08MSD3", "T09MOD3", "T09MSD3",
  "T10MOD3", "T10MSD3", "T11MOD3", "T11MSD3", "T12MOD3", "T12MSD3", "TMDMOD3", "TMNMOD3",
  "TPIMRG5", "TREL10", "TWIMRG5", "VBFMRG5", "VDPMRG5", "VW1MOD1", "VW2MOD1", "VW3MOD1",
  "VW4MOD1", "VW5MOD1", "VW6MOD1"
)

# Lithology column mapping (from lithology_data actual names to final names)
lithology_mapping <- list(
  "Lthlg_V" = "Lthlg_V",    # Keep as is
  "Litho_s" = "Litho_s",    # Keep as is 
  "Lith_vb" = "Lith_vb",    # Keep as is
  "Lith_ss" = "Lith_ss",    # Keep as is
  "Lith_pb" = "Lith_pb",    # Keep as is
  "Lith_sm" = "Lith_sm",    # Keep as is
  "Lith_sc" = "Lith_sc",    # Keep as is
  "Litho_va" = "Litho_va",  # Keep as is
  "Lith_mt" = "Lith_mt",    # Keep as is
  "Litho_pa" = "Litho_pa",  # Keep as is
  "Litho_vi" = "Litho_vi",  # Keep as is
  "Lith_wb" = "Lith_wb",    # Keep as is
  "Lith_py" = "Lith_py",    # Keep as is
  "Litho_pi" = "Litho_pi",  # Keep as is
  "Litho_ev" = "Litho_ev",  # Keep as is
  "Lith_nd" = "Lith_nd",    # Keep as is
  "Litho_g" = "Litho_g"     # Keep as is
)

# Create final dataset
# --------------------
cat("Creating final dataset...\n")

# Start with basic columns from lithology data (has the correct abbreviated names)
final_data <- lithology_data %>%
  select(all_of(unlist(final_columns))) %>%
  rename(!!!final_columns)

# Add ISRIC covariate columns (keep same names)
for (col in isric_covariates) {
  if (col %in% names(isric_data)) {
    final_data[[col]] <- isric_data[[col]]
  } else {
    cat(paste("Warning: Column", col, "not found in ISRIC data\n"))
  }
}

# Add lithology columns with renaming
for (new_name in names(lithology_mapping)) {
  old_name <- lithology_mapping[[new_name]]
  if (old_name %in% names(lithology_data)) {
    final_data[[new_name]] <- lithology_data[[old_name]]
  } else {
    cat(paste("Warning: Column", old_name, "not found in lithology data\n"))
  }
}

# Verify final dataset structure
# ------------------------------
cat("Final dataset created with", nrow(final_data), "rows and", ncol(final_data), "columns\n")

# Check that we have all expected columns
expected_cols <- c(
  names(final_columns),
  isric_covariates,
  names(lithology_mapping)
)

missing_cols <- setdiff(expected_cols, names(final_data))
if (length(missing_cols) > 0) {
  cat("Warning: Missing columns in final dataset:\n")
  print(missing_cols)
}

extra_cols <- setdiff(names(final_data), c(expected_cols, "geometry"))
if (length(extra_cols) > 0) {
  cat("Extra columns in final dataset:\n")
  print(extra_cols)
}

# Save results
# ------------
cat("Saving final results...\n")

# Save as shapefile
st_write(final_data, output_shp, delete_dsn = TRUE)
cat(paste("Final shapefile saved to:", output_shp, "\n"))

# Save as CSV (without geometry)
final_data_df <- st_drop_geometry(final_data)
write.csv(final_data_df, output_csv, row.names = FALSE)
cat(paste("Final CSV saved to:", output_csv, "\n"))

# Summary
# -------
cat("\n=== MERGE COMPLETE ===\n")
cat(paste("Total points:", nrow(final_data), "\n"))
cat(paste("Total columns:", ncol(final_data), "including geometry\n"))
cat(paste("Climate variables:", length(unlist(final_columns)), "\n"))
cat(paste("ISRIC covariates:", length(isric_covariates), "\n"))
cat(paste("Lithology variables:", length(lithology_mapping), "\n"))

# Show first few column names
cat("\nFirst 10 column names:\n")
print(head(names(final_data), 10))

cat("\nLast 10 column names:\n")
print(tail(names(final_data), 10))

cat("\nScript completed successfully!\n")