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
│   └── 3_Regressions.R
├── data/
│   ├── Data_All_Compiled.csv    # Survey data
│   └── Species_Ranges.csv       # Species elevational ranges
└── results/
    ├── figs/
    ├── 2-PredictorSummary-by-Mountain.csv
    ├── 2-PredictorSpread-Toiyabe-minus-Ruby.csv
    └── 3-*.txt                  # Model summaries and AICc comparisons
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
| Elevation | Elevation in meters above sea level |
| TrapNights | Sampling effort |
| Captures | Total mammal captures |
| TmaxAvg | Average daily maximum temperature, warmest quarter (°C) |
| PPTAvg | Average total winter–spring precipitation (mm) |
| landfire_H_habitat_250 | Shannon habitat diversity index (250 m buffer) |
| mean_evi_above_threshold | Enhanced Vegetation Index (growing season mean) |
| Width | Canyon width, ridgeline to ridgeline (m) |
| Relief | Canyon relief, ridgeline to river channel (m) |
| Hypsometric.Integral | Proportion of area above normalized elevation in each band |
| Roughness | Surface roughness from 10 m DEM |
| **RangeThru** | **Response variable**: range-through species richness |

### `data/Species_Ranges.csv`
Minimum and maximum confirmed elevation (m) for each small mammal species by mountain range.

---

## Code

All scripts are sourced sequentially by `RunMe.R`.

### `RunMe.R`
Sets working directories, loads libraries (`mgcv`, `bbmle`), defines shared plotting parameters (colors, line styles), and sources the three analysis scripts in order.

### Script 1 — Species Ranges and Richness Visualization
- Plots each species' elevational range as arrows, sorted by minimum elevation, separately for each mountain
- Plots range-through species richness vs. elevation with a GAM smoothing spline and 95% confidence band

### Script 2 — Predictor Variable Plots
- Plots all eight predictor variables vs. elevation with GAM smoothing fits and 95% confidence bands, grouped by category (climate, habitat, geology)
- Computes and saves summary statistics (min, max, spread) for each predictor by mountain

### Script 3 — Multiple Regression Analyses
- Fits linear and non-linear (GAM with smoothing splines, k = 3) models for three predictor groupings: all predictors, environmental only, geomorphological only
- All models run in both natural units and z-score standardized (SD) units
- Selects among models using AICc (corrected Akaike Information Criterion) with model weights
- Produces plots of standardized regression coefficients with 95% confidence intervals
- Produces residual diagnostic plots (predicted − observed richness vs. elevation)

---

## Dependencies

| Package | Use |
|---------|-----|
| `mgcv` | Generalized additive models (GAM) with REML estimation |
| `bbmle` | AICc-based model comparison and model weights |

Install with:

```r
install.packages(c("mgcv", "bbmle"))
```

---

## Authors

- Rebecca C. Terry — Oregon State University (rebecca.terry@oregonstate.edu)
- Brian K. Yanites — Indiana University, Bloomington
- Jesse A. Laney — Portland State University
- Rebecca J. Rowe — University of New Hampshire

---
---