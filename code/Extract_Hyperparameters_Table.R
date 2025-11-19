################################################################################
# EXTRACT HYPERPARAMETER TABLE
# 
# Purpose: Extract optimal hyperparameter values from saved model objects
# and create Table X for the manuscript (Supplementary Materials)
#
# Output:
# - Table X as markdown format (out/Table_X_Hyperparameters.md)
# - Table X as CSV format (out/Table_X_Hyperparameters.csv)
################################################################################

# Clear workspace
rm(list = ls())

# Set working directory
setwd("/Users/ej/CFFRC/04-Research/Soil/depth SLK")

# Load required libraries
suppressPackageStartupMessages({
    library(dplyr)
    library(readr)
})

# Load the saved model list
cat("Loading saved model objects...\n")
model_list <- readRDS("out/final_model_list.rds")

# Extract bestTune for each model
rf_best <- model_list$rf$bestTune
xgb_best <- model_list$xgbTree$bestTune
cubist_best <- model_list$cubist$bestTune
svm_best <- model_list$svmRadial$bestTune

# Create the hyperparameter table
cat("Creating hyperparameter table...\n")

# Define the ranges and descriptions from the paper
hyperparam_info <- list(
    "Random Forest" = list(
        mtry = list(range = "~5-10 (varies by caret version)", 
                   description = "Number of variables sampled at each split")
    ),
    "XGBoost" = list(
        nrounds = list(range = "50-200", description = "Boosting iterations"),
        max_depth = list(range = "1-6", description = "Maximum tree depth"),
        eta = list(range = "0.1-0.4", description = "Learning rate"),
        gamma = list(range = "0-0.1", description = "Minimum loss reduction"),
        colsample_bytree = list(range = "0.6-1.0", description = "Column subsampling ratio"),
        min_child_weight = list(range = "0-10", description = "Minimum child weight"),
        subsample = list(range = "0.5-1.0", description = "Row subsampling ratio")
    ),
    "Cubist" = list(
        committees = list(range = "1-100", description = "Number of model trees"),
        neighbors = list(range = "0-9", description = "Nearest neighbors for corrections")
    ),
    "SVM (Radial)" = list(
        sigma = list(range = "Auto-estimated (3 values around estimate)", description = "RBF kernel width"),
        C = list(range = "0.25-4.0 (log scale)", description = "Cost parameter")
    )
)

# Helper function to format optimal values
format_optimal_value <- function(value) {
    if (is.numeric(value)) {
        # Handle zero values
        if (value == 0) {
            return("0")
        }
        # Round to 4 decimal places for display, but keep more precision if needed
        if (abs(value) < 0.01) {
            return(sprintf("%.6f", value))
        } else if (abs(value) < 1) {
            return(sprintf("%.4f", value))
        } else if (value == round(value)) {
            return(as.character(round(value)))
        } else {
            return(sprintf("%.2f", value))
        }
    }
    return(as.character(value))
}

# Build the table
table_rows <- list()

# Random Forest
if ("mtry" %in% names(rf_best)) {
    table_rows[[length(table_rows) + 1]] <- data.frame(
        Model = "Random Forest",
        Hyperparameter = "mtry",
        Range_Explored = hyperparam_info[["Random Forest"]][["mtry"]][["range"]],
        Optimal_Value = format_optimal_value(rf_best$mtry),
        Description = hyperparam_info[["Random Forest"]][["mtry"]][["description"]],
        stringsAsFactors = FALSE
    )
}

# XGBoost
xgb_params <- c("nrounds", "max_depth", "eta", "gamma", "colsample_bytree", "min_child_weight", "subsample")
for (param in xgb_params) {
    if (param %in% names(xgb_best)) {
        table_rows[[length(table_rows) + 1]] <- data.frame(
            Model = ifelse(param == "nrounds", "XGBoost", ""),
            Hyperparameter = param,
            Range_Explored = hyperparam_info[["XGBoost"]][[param]][["range"]],
            Optimal_Value = format_optimal_value(xgb_best[[param]]),
            Description = hyperparam_info[["XGBoost"]][[param]][["description"]],
            stringsAsFactors = FALSE
        )
    }
}

# Cubist
cubist_params <- c("committees", "neighbors")
for (param in cubist_params) {
    if (param %in% names(cubist_best)) {
        table_rows[[length(table_rows) + 1]] <- data.frame(
            Model = ifelse(param == "committees", "Cubist", ""),
            Hyperparameter = param,
            Range_Explored = hyperparam_info[["Cubist"]][[param]][["range"]],
            Optimal_Value = format_optimal_value(cubist_best[[param]]),
            Description = hyperparam_info[["Cubist"]][[param]][["description"]],
            stringsAsFactors = FALSE
        )
    }
}

# SVM
svm_params <- c("sigma", "C")
for (param in svm_params) {
    if (param %in% names(svm_best)) {
        table_rows[[length(table_rows) + 1]] <- data.frame(
            Model = ifelse(param == "sigma", "SVM (Radial)", ""),
            Hyperparameter = param,
            Range_Explored = hyperparam_info[["SVM (Radial)"]][[param]][["range"]],
            Optimal_Value = format_optimal_value(svm_best[[param]]),
            Description = hyperparam_info[["SVM (Radial)"]][[param]][["description"]],
            stringsAsFactors = FALSE
        )
    }
}

# Combine all rows
hyperparameter_table <- bind_rows(table_rows)

# Save as CSV
write_csv(hyperparameter_table, "out/Table_X_Hyperparameters.csv")
cat("CSV table saved to 'out/Table_X_Hyperparameters.csv'\n")

# Create markdown table
cat("\nCreating markdown table...\n")
md_table <- "## Table X: Hyperparameter Tuning Details\n\n"
md_table <- paste0(md_table, "Hyperparameter tuning details for machine learning models. ")
md_table <- paste0(md_table, "All models were tuned using 10-fold repeated cross-validation (3 repeats) with `tuneLength = 3`.\n\n")
md_table <- paste0(md_table, "| Model | Hyperparameter | Range Explored | Optimal Value | Description |\n")
md_table <- paste0(md_table, "|-------|----------------|----------------|---------------|-------------|\n")

for (i in 1:nrow(hyperparameter_table)) {
    row <- hyperparameter_table[i, ]
    md_table <- paste0(md_table, "| ", row$Model, " | ", row$Hyperparameter, " | ", 
                      row$Range_Explored, " | ", row$Optimal_Value, " | ", 
                      row$Description, " |\n")
}

# Write markdown file
writeLines(md_table, "out/Table_X_Hyperparameters.md")
cat("Markdown table saved to 'out/Table_X_Hyperparameters.md'\n")

# Print to console
cat("\n=== Table X: Hyperparameter Tuning Details ===\n")
print(hyperparameter_table, row.names = FALSE)

cat("\n=== Extraction complete ===\n")

