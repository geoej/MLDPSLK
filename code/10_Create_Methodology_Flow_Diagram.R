################################################################################
# SOIL DEPTH ANALYSIS - SCRIPT 10
# CREATE METHODOLOGY FLOW DIAGRAM
#
# Purpose:
# 1. Generate a publication-quality flow diagram of the methodology
#
# Output:
# - Flow diagram saved as 'out/Figure_Methodology_Flow_Diagram.png'
################################################################################

# --- 1. Environment Setup ---
cat("--- 1. Setting up the environment ---\n")

# Clear workspace
rm(list = ls())

# Set working directory
setwd("/Users/ej/CFFRC/04-Research/Soil/depth SLK")

# Load required libraries
suppressPackageStartupMessages({
    library(DiagrammeR)
    library(magrittr)
    library(htmlwidgets)
})

# --- 2. Create Flow Diagram ---
cat("--- 2. Creating methodology flow diagram ---\n")

# Create a mermaid diagram
flow_diagram <- "
graph TD
    A[96 Soil Profile Observations<br/>Depth to Bedrock] --> B[Data Pre-processing<br/>Log Transformation<br/>Spatial Projection]
    C[247 Environmental Covariates<br/>Climate, Topography,<br/>Soil Properties, Lithology] --> B
    B --> D[Feature Selection<br/>Random Forest<br/>Variable Importance]
    D --> E[Top 24 Predictors<br/>Selected]
    E --> F[Model Training<br/>RF, XGBoost, Cubist, SVM]
    F --> G[Hyperparameter Tuning<br/>10-fold Repeated CV]
    G --> H[Leave-One-Out<br/>Cross-Validation]
    H --> I[Spatial Cross-Validation<br/>5 km Buffer]
    I --> J[Stacked Ensemble Model<br/>GLM Meta-model]
    J --> K[Final Prediction Map<br/>250 m Resolution]
    J --> L[Uncertainty Maps<br/>SD and 95% PI]
    H --> M[Performance Metrics<br/>R², RMSE, ME]
    I --> M
    K --> N[Validation<br/>vs. Global Products]
    L --> N
    M --> O[Final Soil Depth Map<br/>for Sri Lanka]
    N --> O
    
    style A fill:#e1f5ff
    style C fill:#e1f5ff
    style E fill:#fff4e1
    style J fill:#e8f5e9
    style O fill:#f3e5f5
"

# Alternative: Create using DiagrammeR for better control
# We'll use a simpler approach with ggplot2 and grid for publication quality

# --- 3. Create Diagram Using ggplot2 ---
cat("--- 3. Creating publication-quality diagram ---\n")

library(ggplot2)
library(grid)
library(gridExtra)

# Create a data frame for the flow diagram
flow_data <- data.frame(
    step = c("Input Data", "Pre-processing", "Feature Selection", 
             "Model Training", "Validation", "Ensemble", "Final Output"),
    x = c(1, 2, 3, 4, 5, 6, 7),
    y = rep(1, 7),
    details = c(
        "96 profiles\n247 covariates",
        "Log transform\nSpatial projection",
        "RF importance\nTop 24 selected",
        "4 ML algorithms\nHyperparameter tuning",
        "LOOCV + Spatial CV\nPerformance metrics",
        "Stacked ensemble\nGLM meta-model",
        "250 m map\nUncertainty maps"
    )
)

# Create the diagram
flow_plot <- ggplot(flow_data) +
    # Boxes
    geom_rect(aes(xmin = x - 0.4, xmax = x + 0.4, ymin = y - 0.3, ymax = y + 0.3),
              fill = "lightblue", color = "black", alpha = 0.7) +
    # Step labels
    geom_text(aes(x = x, y = y + 0.15, label = step), fontface = "bold", size = 3.5) +
    # Details
    geom_text(aes(x = x, y = y - 0.1, label = details), size = 2.5) +
    # Arrows (except for last box)
    geom_segment(data = flow_data[-nrow(flow_data), ],
                 aes(x = x + 0.4, xend = x + 1 - 0.4, y = y, yend = y),
                 arrow = arrow(length = unit(0.2, "cm")), linewidth = 1) +
    xlim(0.5, 7.5) +
    ylim(0.5, 1.5) +
    theme_void() +
    theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5)) +
    labs(title = "Digital Soil Mapping Workflow")

ggsave("out/Figure_Methodology_Flow_Diagram.png", flow_plot, 
       width = 14, height = 4, dpi = 300, bg = "white")

cat("Flow diagram saved to 'out/Figure_Methodology_Flow_Diagram.png'\n")

# --- 4. Create More Detailed Diagram ---
cat("--- 4. Creating detailed flow diagram ---\n")

# Create a more detailed version with branching
detailed_flow <- ggplot() +
    # Input boxes
    annotate("rect", xmin = 0.5, xmax = 1.5, ymin = 0.5, ymax = 1.5, 
             fill = "lightblue", alpha = 0.7, color = "black") +
    annotate("text", x = 1, y = 1.2, label = "Input Data", fontface = "bold", size = 4) +
    annotate("text", x = 1, y = 0.9, label = "96 soil profiles", size = 3) +
    annotate("text", x = 1, y = 0.7, label = "247 covariates", size = 3) +
    
    # Pre-processing
    annotate("rect", xmin = 2, xmax = 3, ymin = 0.5, ymax = 1.5,
             fill = "lightgreen", alpha = 0.7, color = "black") +
    annotate("text", x = 2.5, y = 1.2, label = "Pre-processing", fontface = "bold", size = 4) +
    annotate("text", x = 2.5, y = 0.9, label = "Log transformation", size = 3) +
    annotate("text", x = 2.5, y = 0.7, label = "UTM projection", size = 3) +
    
    # Feature Selection
    annotate("rect", xmin = 3.5, xmax = 4.5, ymin = 0.5, ymax = 1.5,
             fill = "lightyellow", alpha = 0.7, color = "black") +
    annotate("text", x = 4, y = 1.2, label = "Feature Selection", fontface = "bold", size = 4) +
    annotate("text", x = 4, y = 0.9, label = "RF importance", size = 3) +
    annotate("text", x = 4, y = 0.7, label = "Top 24 selected", size = 3) +
    
    # Model Training
    annotate("rect", xmin = 5, xmax = 6, ymin = 0.5, ymax = 1.5,
             fill = "lightcoral", alpha = 0.7, color = "black") +
    annotate("text", x = 5.5, y = 1.2, label = "Model Training", fontface = "bold", size = 4) +
    annotate("text", x = 5.5, y = 0.9, label = "RF, XGBoost, Cubist, SVM", size = 3) +
    annotate("text", x = 5.5, y = 0.7, label = "Hyperparameter tuning", size = 3) +
    
    # Validation
    annotate("rect", xmin = 6.5, xmax = 7.5, ymin = 0.5, ymax = 1.5,
             fill = "lightpink", alpha = 0.7, color = "black") +
    annotate("text", x = 7, y = 1.2, label = "Validation", fontface = "bold", size = 4) +
    annotate("text", x = 7, y = 0.9, label = "LOOCV + Spatial CV", size = 3) +
    annotate("text", x = 7, y = 0.7, label = "R², RMSE, ME", size = 3) +
    
    # Ensemble
    annotate("rect", xmin = 8, xmax = 9, ymin = 0.5, ymax = 1.5,
             fill = "lavender", alpha = 0.7, color = "black") +
    annotate("text", x = 8.5, y = 1.2, label = "Ensemble", fontface = "bold", size = 4) +
    annotate("text", x = 8.5, y = 0.9, label = "Stacked model", size = 3) +
    annotate("text", x = 8.5, y = 0.7, label = "GLM meta-model", size = 3) +
    
    # Output
    annotate("rect", xmin = 9.5, xmax = 10.5, ymin = 0.5, ymax = 1.5,
             fill = "plum", alpha = 0.7, color = "black") +
    annotate("text", x = 10, y = 1.2, label = "Output", fontface = "bold", size = 4) +
    annotate("text", x = 10, y = 0.9, label = "250 m depth map", size = 3) +
    annotate("text", x = 10, y = 0.7, label = "Uncertainty maps", size = 3) +
    
    # Arrows
    annotate("segment", x = 1.5, xend = 2, y = 1, yend = 1,
             arrow = arrow(length = unit(0.2, "cm")), linewidth = 1.5) +
    annotate("segment", x = 3, xend = 3.5, y = 1, yend = 1,
             arrow = arrow(length = unit(0.2, "cm")), linewidth = 1.5) +
    annotate("segment", x = 4.5, xend = 5, y = 1, yend = 1,
             arrow = arrow(length = unit(0.2, "cm")), linewidth = 1.5) +
    annotate("segment", x = 6, xend = 6.5, y = 1, yend = 1,
             arrow = arrow(length = unit(0.2, "cm")), linewidth = 1.5) +
    annotate("segment", x = 7.5, xend = 8, y = 1, yend = 1,
             arrow = arrow(length = unit(0.2, "cm")), linewidth = 1.5) +
    annotate("segment", x = 9, xend = 9.5, y = 1, yend = 1,
             arrow = arrow(length = unit(0.2, "cm")), linewidth = 1.5) +
    
    xlim(0, 11) +
    ylim(0, 2) +
    theme_void() +
    theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5, margin = margin(b = 20))) +
    labs(title = "Digital Soil Mapping Workflow for Soil Depth Prediction in Sri Lanka")

ggsave("out/Figure_Methodology_Flow_Diagram_Detailed.png", detailed_flow, 
       width = 16, height = 5, dpi = 300, bg = "white")

cat("Detailed flow diagram saved to 'out/Figure_Methodology_Flow_Diagram_Detailed.png'\n")

cat("\n=== SCRIPT 10 COMPLETE: Methodology flow diagram created. ===\n")

