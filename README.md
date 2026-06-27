# Canyon Geomorphology Small Mammal Diversity

Code repository for:

> Terry, R.C., Yanites, B.K., Laney, J.A., and Rowe, R.J. "Topographic Controls on Montane Small Mammal Diversity: Canyon Relief and Width Predict Elevational Richness Peaks."

This project compares canyon geomorphology and traditional environmental variables as predictors of small mammal species richness in surveys from two Great Basin mountain ranges.

---

## Study System

Small mammal communities were surveyed along elevation gradients in two Nevada mountain ranges:

- **Ruby Mountains** (northeastern Nevada) — glacially carved, metamorphic core complex; surveyed 2006–2008
- **Toiyabe Range** (central Nevada) — less glaciated, sedimentary/igneous geology; surveyed 2009–2011

Surveys targeted shrews and rodents (<500 g) across ~1,000 m of vertical relief in each range. Species richness is estimated as **range-through richness**: the number of species whose known elevational ranges encompass each survey point, which accounts for imperfect detection.

---

## Repository Structure

```
Git/
├── code/
│   ├── RunMe.R
│   ├── 1. Range Richness and Abundance Plots - Final.R
│   ├── 2. Predictor Plots - Final.R
│   └── 3. Multiple Regression Analyses - RangeThru Richness - Final.R
├── data/
│   ├── Data_All_Compiled.csv    # Survey data (19 sites × 19 variables)
│   └── Species_Ranges.csv       # Species elevational ranges (30 species)
└── results/
    ├── figs/
    ├── 2-PredictorSummary-by-Mountain.csv
    ├── 2-PredictorSpread-Toiyabe-minus-Ruby.csv
    └── 3-*.txt                  # Model summaries and AICc comparisons
```

---

## Data

### `data/Data_All_Compiled.csv`
Main dataset with one row per survey site (n = 19 sites across both ranges).

| Column | Description |
|--------|-------------|
| Mountain | Range identity: "Ruby" or "Toiyabe" |
| Site2 | Site name |
| Survey Year | Year of survey |
| Elevation | Elevation in meters above sea level |
| TrapNights | Sampling effort |
| Captures | Total mammal captures |
| TmaxAvg | Average daily maximum temperature, warmest quarter (°C) |
| PPTAvg | Average winter–spring precipitation (mm) |
| landfire_H_habitat_250 | Shannon habitat diversity index (250 m buffer) |
| mean_evi_above_threshold | MODIS Enhanced Vegetation Index (growing season mean) |
| Width | Canyon width, ridgeline to ridgeline (m) |
| Relief | Canyon relief, ridgeline to river channel (m) |
| Hypsometric.Integral | Proportion of area above normalized elevation in each band |
| Roughness | Surface roughness from 10 m DEM |
| **RangeThru** | **Response variable**: range-through species richness (5–14) |

### `data/Species_Ranges.csv`
Minimum and maximum confirmed elevation (m) for each of 30 mammal species by mountain range (15 species per range, with some overlap).

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
- Produces forest plots of standardized regression coefficients with 95% confidence intervals
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