# Canyon Geomorphology Small Mammal Diversity

Code repository for:

> Terry, R.C., Yanites, B.K., Laney, J.A., and Rowe, R.J. "Canyon geomorphology predicts elevational diversity in Great Basin small mammals."

This project compares geomorphological metrics and traditional environmental variables as predictors of elevational patterns of small mammal species richness along canyon gradients in two Great Basin mountain ranges.

---

## Study System

Survey data on small mammal communities along elevation gradients in two Nevada mountain ranges were combined with metrics capturing river canyon size and shape. The focal mountain ranges for this analysis are: the Ruby Mountains (northeastern Nevada, surveyed 2006-08) and the Toiyabe Range (central Nevada, surveyed 2009-11).

Surveys targeted shrews and rodents (<500 g) across ~1,000 m of vertical relief in each mountain range. Species richness at each locality within each mountain range is estimated from detection-corrected presence/absence data as **range-through richness**.

---

## Repository Structure

```
Git/
├── code/
│   ├── RunMe.R
│   ├── 1_Range_Richness.R
│   ├── 2_Predictors.R
│   ├── 3_Regressions.R
│   └── 4_Autocorrelation.R
├── data/
│   ├── Data_All_Compiled.csv    # Survey data
│   └── Species_Ranges.csv       # Species elevational ranges
└── results/
    ├── figs/
    ├── 2-PredictorSummary-by-Mountain.csv
    ├── 2-PredictorSpread-Toiyabe-minus-Ruby.csv
    ├── 2-PredictorCorrelationStats.csv
    ├── 2-VarianceInflationFactors.txt
    ├── 3-LinearRegression-NaturalUnits-RangeThru.txt
    ├── 3-NonLinearRegression-NaturalUnits-RangeThru.txt
    ├── 3-AIC(c)comparisons-NaturalUnits-RangeThru.txt
    ├── 3-LinearRegression-SDUnits-RangeThru.txt
    ├── 3-NonLinearRegression-SDUnits-RangeThru.txt
    ├── 3-AIC(c)_comparisons-SDUnits-RangeThru.txt
    ├── 3-AIC(c)_PCA_2Full_1Env_1Geo.txt
    ├── 3-AIC(c)_PCA_1Full_1Env_1Geo.txt
    ├── 3-PCA_VariancePartitioning.txt
    └── 4-MoransI.txt
```

---

## Data

### `data/Data_All_Compiled.csv`
Main dataset with one row per survey site.

| Column | Description |
|--------|-------------|
| Mountain | Range identity: "Ruby" or "Toiyabe" |
| Site2 | Site name |
| Survey Year | Year of survey |
| Date survey start | Start date of trapping session |
| Date survey end | End date of trapping session |
| Latitude | Site latitude in decimal degrees (WGS84) |
| Longitude | Site longitude in decimal degrees (WGS84) |
| Elevation | Elevation in meters above sea level |
| TrapNights | Sampling effort |
| Captures | Total mammal captures |
| TmaxAvg | Average daily maximum temperature, warmest quarter (°C) |
| PPTAvg | Average total winter–spring precipitation (mm) |
| landfire_H_habitat_250 | Shannon habitat diversity index (250 m buffer) |
| mean_evi_above_threshold | Enhanced Vegetation Index (growing season mean) |
| Width | Canyon width, ridgeline to ridgeline (m) |
| Relief | Canyon relief, ridgeline to river channel (m) |
| Hypsometric Integral | Proportion of area above normalized elevation in each band (read into R as `Hypsometric.Integral`) |
| Roughness | Surface roughness from 10 m DEM |
| **RangeThru** | **Response variable**: range-through species richness |

### `data/Species_Ranges.csv`
Minimum and maximum confirmed elevation (m) for each small mammal species by mountain range.

---

## Code

All scripts are sourced sequentially by `RunMe.R`.

### `RunMe.R`
Sets working directories, loads libraries (`mgcv`, `bbmle`, `corrplot`, `car`, `vegan`, `ape`, `geosphere`), defines shared plotting parameters (colors, line styles), and sources the four analysis scripts in order.

### Script 1 — Species Ranges and Richness Visualization
- Plots each species' elevational range as arrows, sorted by minimum elevation, separately for each mountain
- Plots range-through species richness vs. elevation with a GAM smoothing spline and 95% confidence band

### Script 2 — Predictor Variable Plots
- Plots all eight predictor variables vs. elevation with GAM smoothing fits and 95% confidence bands, grouped by category (climate, habitat, geology)
- Computes and saves summary statistics (min, max, spread) for each predictor by mountain
- Produces pairwise correlation matrix plot and computes mean within- and between-group correlations (environmental vs. geomorphological predictors)
- Calculates variance inflation factors (VIFs) for the full, geomorphological, and environmental predictor sets

### Script 3 — Multiple Regression Analyses
- Fits linear and non-linear (GAM with smoothing splines, k = 3) models for three predictor groupings: all predictors, environmental only, geomorphological only
- All models run in both natural units and z-score standardized (SD) units
- Selects among models using AICc (corrected Akaike Information Criterion) with model weights
- Produces plots of standardized regression coefficients with 95% confidence intervals
- Produces residual diagnostic plots (predicted − observed richness vs. elevation)
- Performs PCA-based reanalysis to assess collinearity: retains axes via broken-stick criterion, refits GAMs on PC scores, and computes variance partitioning among geomorphological, environmental, and mountain-identity predictors

### Script 4 — Spatial Autocorrelation Tests
- Refits the linear SD-unit models (all, geomorphological, environmental) on the compiled dataset
- Tests for spatial autocorrelation in model residuals using Moran's I with three weighting schemes: inverse elevational distance (all within-mountain pairs), adjacent-site elevation order, and inverse geographic (great-circle) distance
- Uses a permutation approach (1,000 shuffles) to generate two-sided p-values
- Saves a summary table of observed I, expected I, and permutation p-values for each model and weighting scheme

---

## Dependencies

All analyses were run under **R version 4.6.0** (2026-04-24).

| Package | Version | Use |
|---------|---------|-----|
| `mgcv` | 1.9.4 | Generalized additive models (GAM) with REML estimation |
| `bbmle` | 1.0.25.1 | AICc-based model comparison and model weights |
| `corrplot` | 0.95 | Pairwise correlation matrix visualization (Script 2) |
| `car` | 3.1.5 | Variance inflation factors / `vif()` (Script 2) |
| `vegan` | 2.7.6 | Variance partitioning / `varpart()` and broken-stick scree plots (Script 3) |
| `ape` | 5.8.1 | Moran's I spatial autocorrelation test / `Moran.I()` (Script 4) |
| `geosphere` | 1.6.8 | Great-circle geographic distances / `distm()`, `distHaversine` (Script 4) |

Install with:

```r
install.packages(c("mgcv", "bbmle", "corrplot", "car", "vegan", "ape", "geosphere"))
```

---

## Authors

- Rebecca C. Terry — Oregon State University (terryr@oregonstate.edu)
- Brian K. Yanites — Indiana University, Bloomington
- Jesse A. Laney — Portland State University
- Rebecca J. Rowe — University of New Hampshire

---
---
