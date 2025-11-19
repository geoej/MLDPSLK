################################################################################
# SOIL DEPTH ANALYSIS - SCRIPT 8
# FEATURE SELECTION STABILITY AND CORRELATION ANALYSIS
#
# Purpose:
# 1. Analyze correlation structure of selected 24 predictors
# 2. Calculate Variance Inflation Factors (VIF) for selected predictors
# 3. Assess feature selection stability through bootstrap analysis
# 4. Generate correlation matrix visualization
#
# Output:
# - Correlation matrix of selected predictors
# - VIF analysis results
# - Feature selection stability analysis
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
    library(corrplot)
    library(car)  # for VIF calculation
    library(randomForest)
    library(ggplot2)
    library(viridis)
})

# --- 2. Load Data ---
cat("--- 2. Loading data ---\n")
soil_data <- readRDS("out/soil_data_processed.rds")
top_24_vars <- read_csv("out/top_24_covariates_final.csv", show_col_types = FALSE)$variable

# Get all available covariates
covariates_dir <- "/Users/ej/CFFRC/04-Research/Soil/SLK soil data/Covariates/ISRIC covariates /isric_gsp/Covs"
available_rasters <- tools::file_path_sans_ext(list.files(covariates_dir, pattern = ".tif$"))
all_covariate_cols <- setdiff(names(soil_data), 
                               c("Id", "lat", "lon", "dpth", "log_dpth", "x_utm", "y_utm"))
all_covariate_cols <- intersect(all_covariate_cols, available_rasters)

# --- 3. Correlation Analysis of Selected Predictors ---
cat("--- 3. Analyzing correlation structure of selected predictors ---\n")

# Extract selected predictors
selected_data <- soil_data[, top_24_vars, drop = FALSE]

# Calculate correlation matrix
cor_matrix <- cor(selected_data, use = "complete.obs")

# Find highly correlated pairs (|r| > 0.7)
high_cor_pairs <- which(abs(cor_matrix) > 0.7 & abs(cor_matrix) < 1, arr.ind = TRUE)
high_cor_df <- data.frame(
    Var1 = rownames(cor_matrix)[high_cor_pairs[, 1]],
    Var2 = colnames(cor_matrix)[high_cor_pairs[, 2]],
    Correlation = cor_matrix[high_cor_pairs]
) %>%
    filter(Var1 < Var2) %>%  # Remove duplicates
    arrange(desc(abs(Correlation)))

cat("\nHighly correlated pairs (|r| > 0.7):\n")
print(head(high_cor_df, 20))

# Save correlation matrix
write_csv(as.data.frame(cor_matrix) %>% 
          mutate(Variable = rownames(cor_matrix)) %>%
          select(Variable, everything()),
          "out/selected_predictors_correlation_matrix.csv")

# --- 4. Variance Inflation Factor (VIF) Analysis ---
cat("--- 4. Calculating Variance Inflation Factors (VIF) ---\n")

# VIF requires a linear model, so we'll fit a simple linear model
# Note: VIF is typically used for linear models, but gives insight into multicollinearity
lm_model <- lm(log_dpth ~ ., data = soil_data[, c("log_dpth", top_24_vars)])

# Calculate VIF
vif_values <- vif(lm_model)
vif_df <- data.frame(
    Variable = names(vif_values),
    VIF = as.numeric(vif_values)
) %>%
    arrange(desc(VIF))

cat("\nVIF values for selected predictors:\n")
print(vif_df)

# Identify variables with high VIF (> 5 indicates potential multicollinearity)
high_vif <- vif_df %>% filter(VIF > 5)
cat(paste("\nVariables with VIF > 5 (potential multicollinearity):", nrow(high_vif), "\n"))
if (nrow(high_vif) > 0) {
    print(high_vif)
}

# Save VIF results
write_csv(vif_df, "out/selected_predictors_vif.csv")

# --- 5. Feature Selection Stability Analysis ---
cat("--- 5. Assessing feature selection stability through bootstrap ---\n")

# Perform bootstrap resampling to assess stability
n_bootstrap <- 100
bootstrap_selections <- list()

set.seed(42)
for (i in 1:n_bootstrap) {
    if (i %% 10 == 0) cat(paste("\r  Bootstrap iteration", i, "of", n_bootstrap))
    
    # Bootstrap sample
    boot_indices <- sample(1:nrow(soil_data), replace = TRUE)
    boot_data <- soil_data[boot_indices, ]
    
    # Train RF and get importance
    tryCatch({
        boot_rf <- randomForest(
            x = boot_data[, all_covariate_cols],
            y = boot_data$log_dpth,
            ntree = 500,  # Reduced for speed
            importance = TRUE
        )
        
        boot_importance <- importance(boot_rf, type = 1)
        boot_importance_df <- data.frame(
            Variable = rownames(boot_importance),
            Importance = boot_importance[, 1]
        ) %>%
            arrange(desc(Importance))
        
        # Get top 24
        top_24_boot <- head(boot_importance_df, 24)$Variable
        bootstrap_selections[[i]] <- top_24_boot
    }, error = function(e) {
        # Skip if error
    })
}
cat("\n")

# Calculate selection frequency
all_vars <- unique(unlist(bootstrap_selections))
selection_freq <- sapply(all_vars, function(v) {
    sum(sapply(bootstrap_selections, function(sel) v %in% sel))
})

selection_freq_df <- data.frame(
    Variable = names(selection_freq),
    Frequency = selection_freq,
    Percentage = (selection_freq / length(bootstrap_selections)) * 100
) %>%
    arrange(desc(Frequency))

# Check how many of our selected 24 appear in top selections
our_selected_in_bootstrap <- selection_freq_df %>%
    filter(Variable %in% top_24_vars)

cat("\nStability of selected 24 variables:\n")
cat(paste("Mean selection frequency:", round(mean(our_selected_in_bootstrap$Percentage), 1), "%\n"))
cat(paste("Variables selected in >80% of bootstrap samples:", 
          sum(our_selected_in_bootstrap$Percentage > 80), "out of 24\n"))
cat(paste("Variables selected in >50% of bootstrap samples:", 
          sum(our_selected_in_bootstrap$Percentage > 50), "out of 24\n"))

# Save stability results
write_csv(selection_freq_df, "out/feature_selection_stability.csv")
write_csv(our_selected_in_bootstrap, "out/selected_24_stability_analysis.csv")

# --- 6. Create Visualizations ---
cat("--- 6. Creating visualizations ---\n")

# Correlation matrix plot
png("out/Figure_Selected_Predictors_Correlation.png", width = 12, height = 10, units = "in", res = 300)
corrplot(cor_matrix, method = "color", type = "upper", 
         order = "hclust", tl.cex = 0.6, tl.col = "black",
         title = "Correlation Matrix of Selected 24 Predictors",
         mar = c(0, 0, 2, 0))
dev.off()

# VIF plot
vif_plot <- ggplot(vif_df, aes(x = reorder(Variable, VIF), y = VIF)) +
    geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7) +
    geom_hline(yintercept = 5, linetype = "dashed", color = "red", linewidth = 1) +
    coord_flip() +
    labs(title = "Variance Inflation Factors (VIF) for Selected Predictors",
         subtitle = "VIF > 5 (red line) indicates potential multicollinearity",
         x = "Variable", y = "VIF") +
    theme_minimal(base_size = 10) +
    theme(plot.title = element_text(face = "bold"))

ggsave("out/Figure_Selected_Predictors_VIF.png", vif_plot, width = 10, height = 8, dpi = 300, bg = "white")

# Stability plot
stability_plot <- ggplot(our_selected_in_bootstrap, aes(x = reorder(Variable, Percentage), y = Percentage)) +
    geom_bar(stat = "identity", fill = "darkgreen", alpha = 0.7) +
    geom_hline(yintercept = 50, linetype = "dashed", color = "orange", linewidth = 1) +
    geom_hline(yintercept = 80, linetype = "dashed", color = "red", linewidth = 1) +
    coord_flip() +
    labs(title = "Feature Selection Stability: Bootstrap Analysis",
         subtitle = "Percentage of bootstrap samples in which each variable was selected in top 24",
         x = "Variable", y = "Selection Frequency (%)") +
    theme_minimal(base_size = 10) +
    theme(plot.title = element_text(face = "bold"))

ggsave("out/Figure_Feature_Selection_Stability.png", stability_plot, width = 10, height = 8, dpi = 300, bg = "white")

cat("\n=== SCRIPT 08 COMPLETE: Feature selection analysis finished. ===\n")







