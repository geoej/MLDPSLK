# =============================================================================
# Merge Covariate Shapefiles and Create Final Dataset - Version 2
# =============================================================================
# 
# Purpose: Merge ISRIC GSP covariates and lithology data into final dataset
#          Using a simpler, more robust approach
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

# Print column names to verify
cat("First 10 ISRIC columns:\n")
print(head(names(isric_data), 10))

cat("First 10 lithology columns:\n")
print(head(names(lithology_data), 10))

cat("Last 10 lithology columns:\n")
print(tail(names(lithology_data), 10))

# Define the exact columns we want in the final output
# ---------------------------------------------------
required_columns <- c(
  # Basic info (use lithology data names as they're already abbreviated)
  "Id", "Latitud", "Longitd", "Dpth___",
  
  # Climate variables (from lithology data as it has same source data but abbreviated names)
  "srad1", "srad2", "srad3", "srad4", "srad5", "srad6", "srad7", "srad8", 
  "srad9", "srad10", "srad11", "srad12", 
  "vapr1", "vapr2", "vapr3", "vapr4", "vapr5", "vapr6", "vapr7", "vapr8", 
  "vapr9", "vapr10", "vapr11", "vapr12",
  "wind1", "wind2", "wind3", "wind4", "wind5", "wind6", "wind7", "wind8", 
  "wind9", "wind10", "wind11", "wind12",
  "tmin1", "tmin2", "tmin3", "tmin4", "tmin5", "tmin6", "tmin7", "tmin8", 
  "tmin9", "tmin10", "tmin11", "tmin12",
  "tmax1", "tmax2", "tmax3", "tmax4", "tmax5", "tmax6", "tmax7", "tmax8", 
  "tmax9", "tmax10", "tmax11", "tmax12",
  "elevatn",
  
  # ISRIC covariates (from ISRIC data)
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
  "VW4MOD1", "VW5MOD1", "VW6MOD1",
  
  # Lithology variables (from lithology data)
  "Lthlg_V", "Litho_s", "Lith_vb", "Lith_ss", "Lith_pb", "Lith_sm", "Lith_sc", 
  "Litho_va", "Lith_mt", "Litho_pa", "Litho_vi", "Lith_wb", "Lith_py", 
  "Litho_pi", "Litho_ev", "Lith_nd", "Litho_g"
)

# Check which columns exist in which dataset
# ------------------------------------------
cat("Checking column availability...\n")

# Basic info and climate columns - check in lithology data first
basic_climate_cols <- required_columns[1:65]  # First 65 columns
available_in_lithology <- intersect(basic_climate_cols, names(lithology_data))
cat(paste("Found", length(available_in_lithology), "basic/climate columns in lithology data\n"))

# ISRIC covariate columns - check in ISRIC data
isric_cols <- required_columns[66:237]  # ISRIC covariate columns
available_in_isric <- intersect(isric_cols, names(isric_data))
cat(paste("Found", length(available_in_isric), "ISRIC covariate columns in ISRIC data\n"))

# Lithology columns - check in lithology data
lithology_cols <- required_columns[238:254]  # Lithology columns
available_lithology <- intersect(lithology_cols, names(lithology_data))
cat(paste("Found", length(available_lithology), "lithology columns in lithology data\n"))

# List actual lithology column names found in lithology data
actual_lithology_cols <- names(lithology_data)[grepl("Lthlg|Litho|Lith_", names(lithology_data))]
cat("Actual lithology columns found:\n")
print(actual_lithology_cols)

# Create final dataset by combining columns from both sources
# ----------------------------------------------------------
cat("Creating final dataset...\n")

# Start with basic info and climate data from lithology data
basic_data <- lithology_data %>%
  select(all_of(available_in_lithology))

# Add ISRIC covariate columns
for (col in available_in_isric) {
  basic_data[[col]] <- isric_data[[col]]
}

# Add ALL lithology columns that exist (use actual column names found)
for (col in actual_lithology_cols) {
  if (col %in% names(lithology_data) && !col %in% names(basic_data)) {
    basic_data[[col]] <- lithology_data[[col]]
  }
}

# Final dataset
final_data <- basic_data

# Verify final dataset structure
# ------------------------------
cat("Final dataset created with", nrow(final_data), "rows and", ncol(final_data), "columns\n")

# Show which columns we have vs requested
found_cols <- intersect(required_columns, names(final_data))
missing_cols <- setdiff(required_columns, names(final_data))

cat(paste("Successfully included", length(found_cols), "out of", length(required_columns), "requested columns\n"))

if (length(missing_cols) > 0) {
  cat("Missing columns:\n")
  print(missing_cols)
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
cat(paste("Columns successfully included:", length(found_cols), "/", length(required_columns), "\n"))

# Show first and last column names
cat("\nFirst 10 column names:\n")
print(head(names(final_data), 10))

cat("\nLast 10 column names:\n")
print(tail(names(final_data), 10))

cat("\nScript completed successfully!\n")