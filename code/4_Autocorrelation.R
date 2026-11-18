######################################################
## Spatial Autocorrelation Test
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Linear models for all combined, geo only, env only, using z-transformed (SD) units

out.all.lin.units <- gam(
  RangeThru ~
    Mountain +
    Width +
    Relief +
    Hypsometric.Integral +
    Roughness +
    mean_evi_above_threshold +
    TmaxAvg +
    PPTAvg +
    landfire_H_habitat_250,
  data = zdata, 
  method = 'REML'
)

out.geo.lin.units <- gam(RangeThru ~
                           Mountain +
                           Width +
                           Relief +
                           Hypsometric.Integral +
                           Roughness,
                         data = zdata, 
                         method = 'REML')

out.env.lin.units <- gam(
  RangeThru ~
    Mountain +
    mean_evi_above_threshold +
    TmaxAvg +
    PPTAvg +
    landfire_H_habitat_250,
  data = zdata, 
  method = 'REML'
)

# MORAN'S I BY ELEVATIONAL DISTANCE BETWEEN SITES

#inverse-distance weights, zero on the diagonal, zero between mountains. Tests for influence across all within-mountain pairs. (But can dilute a signal that exists only between adjacent sites.)

d <- as.matrix(dist(data$Elevation)) #calculates pairwise |elevation difference| in m
w <- 1/d #closer in elev = larger weight
diag(w) <- 0 # weights same site comparisons as zero - a site cannot be its own neighbor
same_mtn <- outer(data$Mountain, data$Mountain, "==") #creates matrix of TRUE/FALSE for sites being from the same mountain
w[!same_mtn] <- 0 #removes weights between sites from different mountains, so transects are not treated as neighbors.  With this only within-mountain pairs count towards Moran's I.

# --- Moran's I on model residuals, with a permutation p-value ---
moran_test <- function(model, w, nperm = 1000) {
  set.seed(1)
  r    <- residuals(model, type = "response")       # residuals: observed minus fitted richness
  obs  <- Moran.I(r, w)$observed                    # observed Moran's I for these residuals
  perm <- replicate(nperm,                          # null distribution: repeat nperm times...
                    Moran.I(sample(r), w)$observed) # ...shuffle residuals among sites, recompute I
  ctr  <- mean(perm)                                # center of the null distribution (about -1/(n-1))
  c(I        = obs,                                 # observed I (positive = neighbors more similar than chance)
    expected = -1 / (length(r) - 1),                # analytic expected I under no autocorrelation
    p_perm   = (1 + sum(abs(perm - ctr) >= abs(obs - ctr))) / (nperm + 1))
  # two-sided p: share of shuffles at least as far
  # from the null center as the observed I,
  # counting the observed value as one arrangement
}

elev_dist <- sapply(list(geo = out.geo.lin.units, 
            env = out.env.lin.units, 
            full = out.all.lin.units),
       moran_test, w = w)

#adjacent-site weights:  each site's neighbors are the sites immediately above and below it in elevation

w_adj <- matrix(0, nrow(data), nrow(data))
for (m in unique(data$Mountain)) {
  idx <- which(data$Mountain == m)
  idx <- idx[order(data$Elevation[idx])]          # sites in this mountain, ordered by elevation
  for (k in seq_len(length(idx) - 1)) {
    w_adj[idx[k], idx[k + 1]] <- 1               # link each site to the next one up
    w_adj[idx[k + 1], idx[k]] <- 1
  }
}

sum(w_adj)   # should be 36: 9 adjacent pairs per mountain x 2 mountains x 2 directions

elev_dist_adjacent <- sapply(list(geo = out.geo.lin.units, 
            env = out.env.lin.units, 
            full = out.all.lin.units),
       moran_test, w = w_adj)

#geographic distance weights

# pairwise great-circle distance in meters between sites (note: Longitude first, then Latitude)
d_geo <- distm(cbind(data$Longitude, data$Latitude), fun = distHaversine)

#inverse-distance weights, zero on the diagonal, zero between mountains. 
w_geo <- 1 / d_geo                                  # closer sites get larger weights
diag(w_geo) <- 0                                    # a site is not its own neighbor
same_mtn <- outer(data$Mountain, data$Mountain, "==")
w_geo[!same_mtn] <- 0                               # no weight between the two mountains

geog_dist <- sapply(list(geo = out.geo.lin.units, 
                         env = out.env.lin.units, 
                         full = out.all.lin.units),
       moran_test, w = w_geo)

# Don't need a geographic adjacent-site test.  In the Toiyabe, distance from the lowest site increases monotonically with elevation so elevation order is in the order along the canyon.  The Rubies follow the same patter everywhere except for at the top, where the 2949 site is geogrpahically closer to the base (because the canyon curves and this is a straight line distance measure) than the 2944 m site, by just a bit (350 m).  So w_adj is a close (but not exact) approximation of adjacent along the canyon.

### Output Results Tables

sink(paste0(resultsdir,'4-MoransI.txt'))
print("ELEVATIONAL DISTANCE - INVERSE DISTANCE WEIGHTING")
print(elev_dist)
print("")
print('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~')
print("ELEVATIONAL DISTANCE - ADJACENT SITE DISTANCE WEIGHTING")
print(elev_dist_adjacent)
print("")
print('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~')
print("GEOGRAPHIC DISTANCE - INVERSE DISTANCE WEIGHTING")
print(geog_dist)
sink()
