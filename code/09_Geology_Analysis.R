################################################################################
# SOIL DEPTH ANALYSIS - SCRIPT 9
# GEOLOGY/LITHOLOGY DATA ANALYSIS
#
# Purpose:
# 1. Assess whether GLiM lithology data was included in the final model
# 2. Quantify potential impact of missing geology data
# 3. Document GLiM data processing workflow
#
# Output:
# - Analysis of geology data availability and usage
# - Documentation of why GLiM was excluded from final model
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
})

# --- 2. Check Final Data for Lithology Variables ---
cat("--- 2. Checking final data for lithology variables ---\n")
soil_data <- readRDS("out/soil_data_processed.rds")
top_24_vars <- read_csv("out/top_24_covariates_final.csv", show_col_types = FALSE)$variable

# Check if any lithology variables are in the selected top 24
lithology_in_selected <- grepl("Litho|Lthlg|Lith_", top_24_vars, ignore.case = TRUE)
cat(paste("Lithology variables in top 24 selected:", sum(lithology_in_selected), "\n"))

# Check if any lithology variables exist in the full dataset
all_lithology_vars <- names(soil_data)[grepl("Litho|Lthlg|Lith_", names(soil_data), ignore.case = TRUE)]
cat(paste("Total lithology variables in dataset:", length(all_lithology_vars), "\n"))
if (length(all_lithology_vars) > 0) {
    cat("Lithology variables found:\n")
    print(all_lithology_vars)
}

# --- 3. Check GLiM Data Availability ---
cat("--- 3. Checking GLiM data availability ---\n")
glim_file <- "external data/glim_0point5deg.shp"
if (file.exists(glim_file)) {
    cat("GLiM data file found.\n")
    glim_data <- st_read(glim_file, quiet = TRUE)
    cat(paste("GLiM data contains", nrow(glim_data), "features\n"))
    cat("GLiM column names:\n")
    print(names(glim_data))
} else {
    cat("GLiM data file not found.\n")
}

# --- 4. Document GLiM Class Mapping ---
cat("--- 4. Documenting GLiM class mapping ---\n")
# GLiM lithology classes (from Hartmann & Moosdorf 2012)
glim_classes <- data.frame(
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

write_csv(glim_classes, "out/glim_class_mapping.csv")
cat("GLiM class mapping saved to 'out/glim_class_mapping.csv'\n")

# --- 5. Assess Impact of Missing Geology ---
cat("--- 5. Assessing potential impact of missing geology data ---\n")

# Check if elevation (which correlates with geology in Sri Lanka) is in top predictors
elevation_in_top <- "DEMENV5" %in% top_24_vars
cat(paste("Elevation (DEMENV5) in top 24:", elevation_in_top, "\n"))

# Note: In Sri Lanka, >90% is Precambrian metamorphic rocks (gneiss and granulite)
# This low geological diversity may limit the value of geology as a predictor
cat("\nNote: Sri Lanka has low geological diversity (>90% Precambrian metamorphic rocks),\n")
cat("which may limit the predictive value of geology data compared to more geologically diverse regions.\n")

cat("\n=== SCRIPT 09 COMPLETE: Geology analysis finished. ===\n")







