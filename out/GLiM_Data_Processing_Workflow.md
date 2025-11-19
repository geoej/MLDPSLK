# GLiM Data Processing Workflow

## Data Source
- **Dataset**: Global Lithological Map (GLiM) version 1.0
- **Reference**: Hartmann, J.; Moosdorf, N. (2012). The new global lithological map database GLiM: A representation of rock properties at the Earth surface. *Geochem. Geophys. Geosyst.*, 13, Q12004.
- **DOI**: https://doi.org/10.1029/2012GC004370
- **Original Resolution**: 0.5° (~50 km at equator)
- **Original Format**: ASCII grid (glim_wgs84_0point5deg.txt.asc)
- **Original CRS**: WGS84 (EPSG:4326)

## Processing Steps

### 1. Data Extraction
- **Input**: GLiM ASCII raster file
- **Method**: Direct extraction at sample point locations using `raster::extract()` function
- **CRS Handling**: Both GLiM data and sample points were in WGS84 (EPSG:4326), so no CRS transformation was required
- **Resampling**: Not required - values were extracted directly at point locations rather than creating a resampled grid

### 2. One-Hot Encoding
- Each lithology class was converted to a binary variable (1 if present at location, 0 if absent)
- This created 17 binary variables:
  - 16 lithology classes (see class mapping table below)
  - 1 "no data" class for locations without lithology information

### 3. Class Mapping
The GLiM dataset uses 16 lithological classes with the following codes and descriptions:

| Code | Description |
|------|-------------|
| su | Unconsolidated sediments |
| vb | Basic volcanic rocks |
| ss | Siliciclastic sedimentary rocks |
| pb | Basic plutonic rocks |
| sm | Mixed sedimentary rocks |
| sc | Carbonate sedimentary rocks |
| va | Acid volcanic rocks |
| mt | Metamorphics |
| pa | Acid plutonic rocks |
| vi | Intermediate volcanic rocks |
| wb | Water bodies |
| py | Pyroclastics |
| pi | Intermediate plutonic rocks |
| ev | Evaporites |
| nd | No data |
| ig | Ice and glaciers |

### 4. Integration with Final Dataset
- Lithology variables were merged with other covariates (climate, topography, soil properties) using spatial join
- Final dataset contained 17 lithology variables (one-hot encoded)
- All variables were included in the initial feature selection pool (247 total variables)

### 5. Feature Selection Result
- **Total lithology variables in dataset**: 17
- **Lithology variables selected in top 24 predictors**: 0
- **Reason for exclusion**: Low geological diversity in Sri Lanka (>90% Precambrian metamorphic rocks) results in limited lithological variation, reducing predictive value relative to climate and topographic variables

## Scripts Used
- **Extraction script**: `code/old/extract_lithology_data.R`
- **Merge script**: `code/merge_covariates_v2.R`
- **Analysis script**: `code/09_Geology_Analysis.R`

## Output Files
- `external data/glim_0point5deg.shp` - Shapefile with lithology data at sample points
- `external data/glim_0point5deg.csv` - CSV with lithology data at sample points
- `out/glim_class_mapping.csv` - Class mapping table

## Reproducibility Notes
- All processing was done in R using standard spatial packages (`raster`, `sf`)
- No custom resampling kernels were used (direct point extraction)
- CRS was consistent throughout (WGS84), so no transformation was needed
- The workflow is fully documented in the extraction script







