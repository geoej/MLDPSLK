# Sri Lanka Depth-to-Bedrock Mapping

This repository contains the R scripts and documentation used to produce a preliminary 250 m resolution map of depth to bedrock (soil depth) for Sri Lanka using Digital Soil Mapping (DSM) and machine-learning ensembles.

## Repository Layout

```
code/                       # Reproducible R scripts for each workflow stage
out/                        # Generated artifacts (maps, tables, figures, logs)
README.md                   # This file
```

## Software Requirements

- R >= 4.3
- R packages (see `code/01_*` for full list): `tidyverse`, `sf`, `terra`, `caret`, `caretEnsemble`, `randomForest`, `xgboost`, `Cubist`, `kernlab`, `ranger`, `ggplot2`, `viridis`, `cowplot`, `tidyterra`, `sp`, `spdep` (optional), `dplyr`, `readr`.
- GDAL/PROJ libraries for spatial data handling.

## Reproducing the Pipeline

1. **Data preparation** (`code/01_Setup_and_Data_Preparation.R`)
   - Loads legacy soil profile data (`data/soil_profiles.csv`) and environmental covariates (`Covariates/`).
   - Produces `out/soil_data_processed.rds` and `out/top_24_covariates_final.csv`.

2. **Modeling & Validation** (`code/02_Modeling_and_Validation.R`)
   - Tunes Random Forest, XGBoost, Cubist, and SVM (radial) via `caret`.
   - Runs leave-one-out CV for base learners and performs nested LOOCV to train the GLM stacking meta-model.
   - Saves tuned models (`out/final_model_list.rds`, `out/final_ensemble_model.rds`), LOOCV metrics, and optimal hyperparameters (`out/optimal_hyperparameters*.{csv,md}`).

3. **Map Generation & Figures** (`code/03_Final_Mapping_and_Visualization.R`)
   - Retrains the stacked ensemble on all 96 profiles.
   - Generates the national 250 m DTB map (`out/Final_Ensemble_Soil_Depth_Map.tif`) and figures for the manuscript.

4. **Global Product Comparison** (`code/05_Global_Products_Comparison.R`)
   - Compares the local ensemble against the Shangguan et al. (2017) global DTB raster.

5. **Spatial Cross-Validation & Uncertainty** (`code/06_Spatial_Cross_Validation.R`)
   - Implements buffered leave-location-out CV (5 km)
   - Exports `out/spatial_cv_summary.csv`, `out/cv_comparison.csv`, and uncertainty rasters (`out/uncertainty_map_sd.tif`, `out/uncertainty_map_pi_{lower,upper}.tif`).

6. **Uncertainty Visualization** (`code/07_Uncertainty_Visualization.R`)
   - Produces publication-ready maps of prediction standard deviation and 95% prediction intervals.

7. **Supporting Analyses**
   - `code/11_Feature_Set_Size_Analysis.R`, `code/12_Sample_Representativeness_Analysis.R`, and `code/13_Diagnostic_Plots_and_Spatial_Autocorrelation.R` provide additional evidence for reviewer responses.

## Re-running Everything

```bash
Rscript code/01_Setup_and_Data_Preparation.R
Rscript code/02_Modeling_and_Validation.R
Rscript code/03_Final_Mapping_and_Visualization.R
Rscript code/05_Global_Products_Comparison.R
Rscript code/06_Spatial_Cross_Validation.R
Rscript code/07_Uncertainty_Visualization.R
Rscript code/11_Feature_Set_Size_Analysis.R
Rscript code/12_Sample_Representativeness_Analysis.R
Rscript code/13_Diagnostic_Plots_and_Spatial_Autocorrelation.R
```

Ensure that the required input data (soil profiles, covariate rasters, and global products) are available in the expected paths described inside each script.

## Outputs

Key outputs are written to the `out/` directory:
- `Final_Ensemble_Soil_Depth_Map.tif`
- `uncertainty_map_sd.tif`, `uncertainty_map_pi_lower.tif`, `uncertainty_map_pi_upper.tif`
- `spatial_cv_summary.csv`, `cv_comparison.csv`
- Figures for the manuscript (`Figure*_*.png`)
- Hyperparameter table (`Table_ST1_Hyperparameters.{csv,md}`)

## Notes

- The map is intended as a **preliminary screening product**. Users must consult the accompanying uncertainty layers and refrain from site-specific engineering or regulatory decisions without ground validation.
- Large raster and intermediate data are not committed to version control; re-run the scripts to regenerate them when needed.
