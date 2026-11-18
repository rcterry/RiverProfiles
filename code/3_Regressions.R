######################################################
## Model Comparisons - River Profile Project
########
## Occupancy Model Range Thru Richness Output
## GAP/Landfire data 2011 raster
## Growing Season EVIs averaged over 3 years
## Updated PRISM data averaged over 3 years
## Both Mountains Combined into one dataset


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Set WD and read in mammal data and compiled predictors

data <- read.csv(paste0(datadir, "Data_All_Compiled.csv"), header = T)
data$col <- NA
data$col[which(data$Mountain == "Ruby")] <- cols2[2]
data$col[which(data$Mountain == "Toiyabe")] <- cols2[1]

# Standardize focal variables to their mean and standard deviation
# Create a new dataset with zscore transformation
zscore <- function(x) {
  (x - mean(x)) / sd(x)
}
variables.to.standardize <- c(
  'Width',
  'Relief',
  'Hypsometric.Integral',
  'Roughness',
  'TmaxAvg',
  'PPTAvg',
  'landfire_H_habitat_250',
  'mean_evi_above_threshold'
)

zdata <- data
zdata[variables.to.standardize] <- apply(zdata[variables.to.standardize], 2, zscore)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Linear models for all combined, geo only, env only, using natural units

out.all.lin.units <- gam(
  RangeThru ~
    Mountain +
    Width +
    Relief +
    Hypsometric.Integral +
    Roughness +
    TmaxAvg +
    PPTAvg +
    landfire_H_habitat_250 +
    mean_evi_above_threshold,
  data = data,
  method = 'REML'
)

out.geo.lin.units <- gam(
  RangeThru ~
    Mountain +
    Width +
    Relief +
    Hypsometric.Integral +
    Roughness,
  data = data,
  method = 'REML'
)

out.env.lin.units <- gam(
  RangeThru ~
    Mountain +
    TmaxAvg +
    PPTAvg +
    landfire_H_habitat_250 +
    mean_evi_above_threshold,
  data = data,
  method = 'REML'
)

sink(
  paste0(
    resultsdir,
    '3-LinearRegression-NaturalUnits-RangeThru.txt'
  )
)
print("LINEAR REGRESSION MODELS WITH NATURAL UNITS - RANGE THRU RICHNESS")
  print("")
  print('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~')
  print("ALL PREDICTORS")
  summary(out.all.lin.units)
  print("")
  print('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~')
  print("GEO PREDICTORS ONLY")
  summary(out.geo.lin.units)
  print("")
  print('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~')
  print("ENV PREDICTORS ONLY")
  summary(out.env.lin.units)
sink()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Nonlinear models for all combined, geo only, env only, using natural units
# knots = 3
kn = 3

out.all.nonlin.units <- gam(
  RangeThru ~
    Mountain +
    s(Width, k = kn) +
    s(Relief, k = kn) +
    s(Hypsometric.Integral, k = kn) +
    s(Roughness, k = kn) +
    s(TmaxAvg, k = kn) +
    s(PPTAvg, k = kn) +
    s(landfire_H_habitat_250, k = kn) +
    s(mean_evi_above_threshold, k = kn),
  data = data,
  method = 'REML'
)

out.geo.nonlin.units <- gam(
  RangeThru ~
    Mountain +
    s(Width, k = kn) +
    s(Relief, k = kn) +
    s(Hypsometric.Integral, k = kn) +
    s(Roughness, k = kn),
  data = data,
  method = 'REML'
)

out.env.nonlin.units <- gam(
  RangeThru ~
    Mountain +
    s(TmaxAvg, k = kn) +
    s(PPTAvg, k = kn) +
    s(landfire_H_habitat_250, k = kn) +
    s(mean_evi_above_threshold, k = kn),
  data = data,
  method = 'REML'
)

sink(
  paste0(
    resultsdir,
    '3-NonLinearRegression-NaturalUnits-RangeThru.txt'
  )
)
print("NON-LINEAR REGRESSION MODELS WITH NATURAL UNITS - RANGE THRU RICHNESS")
print("")
print('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~')
print("ALL PREDICTORS")
summary(out.all.nonlin.units)
print("")
print('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~')
print("GEO PREDICTORS ONLY")
summary(out.geo.nonlin.units)
print("")
print('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~')
print("ENV PREDICTORS ONLY")
summary(out.env.nonlin.units)
sink()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Compare these 6 models with AIC

aicc.linear <- bbmle::AICctab(
  out.all.lin.units,
  out.geo.lin.units,
  out.env.lin.units,
  weights = TRUE,
  base = TRUE
)
aicc.linear

aicc.nonlin <- bbmle::AICctab(
  out.all.nonlin.units,
  out.geo.nonlin.units,
  out.env.nonlin.units,
  weights = TRUE,
  base = TRUE
)
aicc.nonlin

aicc.both <- bbmle::AICctab(
  out.all.lin.units,
  out.geo.lin.units,
  out.env.lin.units,
  out.all.nonlin.units,
  #out.geo.nonlin.units,   #Dropping because it's the same as the linear model
  out.env.nonlin.units,
  weights = TRUE,
  base = TRUE
)
aicc.both


sink(
  paste0(
    resultsdir,
    '3-AIC(c)comparisons-NaturalUnits-RangeThru.txt'
  )
)
print("AIC(c) COMPARISON - NATURAL UNITS - RANGE THRU RICHNESS")
print("")
print('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~')
print("LINEAR MODELS ONLY")
aicc.linear
print("")
print('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~')
print("NONLINEAR MODELS ONLY")
aicc.nonlin
print("")
print('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~')
print("BOTH LINEAR AND NONLINEAR MODELS")
aicc.both
sink()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Linear models for all combined, geo only, env only, using z-transformed (SD) units

out.all.lin.units.sd <- gam(
  RangeThru ~
    Mountain +
    Width +
    Relief +
    Hypsometric.Integral +
    Roughness +
    TmaxAvg +
    PPTAvg +
    landfire_H_habitat_250 +
    mean_evi_above_threshold,
  data = zdata,
  method = 'REML'
)

out.geo.lin.units.sd <- gam(
  RangeThru ~
    Mountain +
    Width +
    Relief +
    Hypsometric.Integral +
    Roughness,
  data = zdata,
  method = 'REML'
)

out.env.lin.units.sd <- gam(
  RangeThru ~
    Mountain +
    TmaxAvg +
    PPTAvg +
    landfire_H_habitat_250 +
    mean_evi_above_threshold,
  data = zdata,
  method = 'REML'
)

sink(paste0(
  resultsdir,
  '3-LinearRegression-SDUnits-RangeThru.txt'
))
print("LINEAR REGRESSION MODELS WITH SD UNITS - RANGE THRU RICHNESS")
print("")
print('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~')
print("ALL PREDICTORS")
summary(out.all.lin.units.sd)
print("")
print('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~')
print("GEO PREDICTORS ONLY")
summary(out.geo.lin.units.sd)
print("")
print('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~')
print("ENV PREDICTORS ONLY")
summary(out.env.lin.units.sd)
sink()

### Set up estimate dataframe for plotting
x <- summary(out.all.lin.units.sd)
y <- data.frame(x$p.table)
colnames(y) <- c("Est", "StdEr", "T.value", "P.value")
y$LCI <- y$Est - 1.96 * y$StdEr
y$UCI <- y$Est + 1.96 * y$StdEr

all <- y
colnames(all) <- paste("All.", colnames(all), sep = "")

x <- summary(out.geo.lin.units.sd)
y <- data.frame(x$p.table)
colnames(y) <- c("Est", "StdEr", "T.value", "P.value")
y$LCI <- y$Est - 1.96 * y$StdEr
y$UCI <- y$Est + 1.96 * y$StdEr

geo <- y
colnames(geo) <- paste("Geo.", colnames(y), sep = "")

x <- summary(out.env.lin.units.sd)
y <- data.frame(x$p.table)
colnames(y) <- c("Est", "StdEr", "T.value", "P.value")
y$LCI <- y$Est - 1.96 * y$StdEr
y$UCI <- y$Est + 1.96 * y$StdEr

env <- y
colnames(env) <- paste("Env.", colnames(y), sep = "")

z <- merge(all, geo, by = "row.names", all = T)
row.names(z) <- z$Row.names
z <- z[, -1]
est.dat <- merge(z, env, by = "row.names", all = T)

est.dat <- est.dat[-1, ]
colnames(est.dat)[1] <- "Predictor"

est.dat$Type <- NA
est.dat$Type[which(
  est.dat$Predictor == "Hypsometric.Integral" |
    est.dat$Predictor == "Relief" |
    est.dat$Predictor == "Roughness" |
    est.dat$Predictor == "Width"
)] <- "Geo"

est.dat$Type[which(
  est.dat$Predictor == "landfire_H_habitat_250" |
    est.dat$Predictor == "mean_evi_above_threshold" |
    est.dat$Predictor == "PPTAvg" |
    est.dat$Predictor == "TmaxAvg"
)] <- "Env"

est.dat$Type[which(est.dat$Predictor == "MountainToiyabe")] <- "Mtn"

est.dat$pch <- NA
est.dat$pch[which(est.dat$Type == "Mtn")] <- 17
est.dat$pch[which(est.dat$Type == "Env")] <- 19
est.dat$pch[which(est.dat$Type == "Geo")] <- 15

# Plotting

order_vec <- c(
  "MountainToiyabe",
  "mean_evi_above_threshold",
  "PPTAvg",
  "TmaxAvg",
  "landfire_H_habitat_250",
  "Relief",
  "Hypsometric.Integral",
  "Roughness",
  "Width"
)

est.dat <- est.dat[match(order_vec, est.dat$Predictor), ]

est.dat$Y <- rev(seq(1, nrow(est.dat)))

xlims = c(-8, 8)
ylims = c(1, 9)
par(mfrow = c(1, 1))

pdf(
  paste0(figdir, "3-Estimates-All.pdf"),
  height = 6,
  width = 3.5,
  useDingbats = FALSE
)
plot(
  est.dat$All.Est,
  est.dat$Y,
  xlim = xlims,
  ylim = ylims,
  type = "n",
  axes = F,
  xlab = "Estimate",
  ylab = ""
)
abline(v = 0, col = "grey", lwd = 1.5)
points(est.dat$All.Est,
       est.dat$Y,
       pch = est.dat$pch,
       cex = 1.5)
arrows(
  est.dat$All.LCI,
  est.dat$Y,
  est.dat$All.UCI,
  est.dat$Y,
  length = 0,
  lwd = 1.5
)
axis(
  1,
  lwd = 1.5,
  labels = seq(-8, 8, 2),
  at = seq(-8, 8, 2),
  tcl = -0.3
)
axis(
  2,
  lwd = 1.5,
  tcl = -0.3,
  labels = as.character(est.dat$Predictor),
  at = est.dat$Y,
  las = 2
)
box(bty = "o", lwd = 2)
dev.off()


pdf(
  paste0(figdir, "3-Estimates-Env.pdf"),
  height = 6,
  width = 3.5,
  useDingbats = FALSE
)
x <- est.dat[which(est.dat$Type == "Env" | est.dat$Type == "Mtn"), ]

plot(
  x$Env.Est,
  x$Y,
  type = "n",
  xlim = xlims,
  ylim = ylims,
  axes = F,
  ylab = "",
  xlab = "Estimate"
)
abline(v = 0, col = "grey", lwd = 1.5)
points(x$Env.Est, x$Y, pch = x$pch, cex = 1.5)
arrows(x$Env.LCI,
       x$Y,
       x$Env.UCI,
       x$Y,
       length = 0,
       lwd = 1.5)
axis(
  1,
  lwd = 1.5,
  labels = seq(-8, 8, 2),
  at = seq(-8, 8, 2),
  tcl = -0.3
)
axis(
  2,
  lwd = 1.5,
  tcl = -0.3,
  labels = as.character(est.dat$Predictor),
  at = est.dat$Y,
  las = 2
)
box(bty = "o", lwd = 2)
dev.off()

pdf(
  paste0(figdir, "3-Estimates-Geo.pdf"),
  height = 6,
  width = 3.5,
  useDingbats = FALSE
)
x <- est.dat[which(est.dat$Type == "Geo" | est.dat$Type == "Mtn"), ]

plot(
  x$Geo.Est,
  x$Y,
  type = "n",
  xlim = xlims,
  ylim = ylims,
  axes = F,
  ylab = "",
  xlab = "Estimate"
)
abline(v = 0, col = "grey", lwd = 1.5)
points(x$Geo.Est, x$Y, pch = x$pch, cex = 1.5)
arrows(x$Geo.LCI,
       x$Y,
       x$Geo.UCI,
       x$Y,
       length = 0,
       lwd = 1.5)
axis(
  1,
  lwd = 1.5,
  labels = seq(-8, 8, 2),
  at = seq(-8, 8, 2),
  tcl = -0.3
)
axis(
  2,
  lwd = 1.5,
  tcl = -0.3,
  labels = as.character(est.dat$Predictor),
  at = est.dat$Y,
  las = 2
)
box(bty = "o", lwd = 2)
dev.off()


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Nonlinear models for all combined, geo only, env only, using SD units
# knots = 3
kn = 3

out.all.nonlin.units.sd <- gam(
  RangeThru ~
    Mountain +
    s(Width, k = kn) +
    s(Relief, k = kn) +
    s(Hypsometric.Integral, k = kn) +
    s(Roughness, k = kn) +
    s(TmaxAvg, k = kn) +
    s(PPTAvg, k = kn) +
    s(landfire_H_habitat_250, k = kn) +
    s(mean_evi_above_threshold, k = kn),
  data = zdata,
  method = 'REML'
)

out.geo.nonlin.units.sd <- gam(
  RangeThru ~
    Mountain +
    s(Width, k = kn) +
    s(Relief, k = kn) +
    s(Hypsometric.Integral, k = kn) +
    s(Roughness, k = kn),
  data = zdata,
  method = 'REML'
)

out.env.nonlin.units.sd <- gam(
  RangeThru ~
    Mountain +
    s(TmaxAvg, k = kn) +
    s(PPTAvg, k = kn) +
    s(landfire_H_habitat_250, k = kn) +
    s(mean_evi_above_threshold, k = kn),
  data = zdata,
  method = 'REML'
)

sink(paste0(
  resultsdir,
  '3-NonLinearRegression-SDUnits-RangeThru.txt'
))
print("NON-LINEAR REGRESSION MODELS WITH SD UNITS - RANGE THRU RICHNESS")
print("")
print('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~')
print("ALL PREDICTORS")
summary(out.all.nonlin.units.sd)
print("")
print('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~')
print("GEO PREDICTORS ONLY")
summary(out.geo.nonlin.units.sd)
print("")
print('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~')
print("ENV PREDICTORS ONLY")
summary(out.env.nonlin.units.sd)
sink()
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Compare these 6 models with AIC

aicc.linear <- AICctab(
  out.all.lin.units.sd,
  out.geo.lin.units.sd,
  out.env.lin.units.sd,
  base = TRUE,
  weights = TRUE
)
aicc.linear

aicc.nonlin <- AICctab(
  out.all.nonlin.units.sd,
  out.geo.nonlin.units.sd,
  out.env.nonlin.units.sd,
  base = TRUE,
  weights = TRUE
)
aicc.nonlin

aicc.both <- AICctab(
  out.all.lin.units.sd,
  out.geo.lin.units.sd,
  out.env.lin.units.sd,
  out.all.nonlin.units.sd,
  #out.geo.nonlin.units.sd,    # Dropping, since same as linear geo
  out.env.nonlin.units.sd,
  base = TRUE,
  weights = TRUE
)
aicc.both

sink(paste0(
  resultsdir,
  '3-AIC(c)_comparisons-SDUnits-RangeThru.txt'
))
print("AIC(c) COMPARISON - SD UNITS - RANGE THRU RICHNESS")
print("")
print('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~')
print("LINEAR MODELS ONLY")
aicc.linear
print("")
print('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~')
print("NONLINEAR MODELS ONLY")
aicc.nonlin
print("")
print('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~')
print("BOTH LINEAR AND NONLINEAR MODELS")
aicc.both
print('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~')
print('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~')
print('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~')
sink()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FOCUS JUST ON LINEAR MODELS
# PLOTS OF RESIDUALS (PREDICTED - OBSERVED RICHNESS)

par(mgp = c(2, 0.5, 0))

data$pred.all <- predict(out.all.lin.units)

pdf(
  paste0(
    figdir,
    "3-Residuals-by-Elevation-FullModel.pdf"
  ),
  width = 5,
  height = 4
)
plot(
  data$Elevation,
  data$pred.all - data$RangeThru,
  xlim = elev.lims,
  xlab = "Elevation (m)",
  ylab = "Richness Residual (Predicted - Observed)",
  axes = F,
  type = 'n',
  cex.lab = cexs.lab
)

abline(h = 0)

points(
  data$Elevation,
  data$pred.all - data$RangeThru,
  pch = 21,
  cex = cexs.pt,
  bg = data$col
)

axis(1,
     tcl = -0.3,
     lwd = 1.5,
     cex.axis = cexs.axis)
axis(
  2,
  las = 0,
  tcl = -0.3,
  lwd = 1.5,
  cex.axis = cexs.axis
)
box(lwd = 2)
dev.off()

#~~~~~~~~~~~~~~~

data$pred.geo <- predict(out.geo.lin.units)

pdf(
  paste0(figdir, "3-Residuals-by-Elevation-GeoModel.pdf"),
  width = 5,
  height = 4
)
plot(
  data$Elevation,
  data$pred.geo - data$RangeThru,
  xlim = elev.lims,
  xlab = "Elevation (m)",
  ylab = "Richness Residual (Predicted - Observed)",
  axes = F,
  type = 'n',
  cex.lab = cexs.lab
)

abline(h = 0)

points(
  data$Elevation,
  data$pred.geo - data$RangeThru,
  pch = 21,
  cex = cexs.pt,
  bg = data$col
)

axis(1,
     tcl = -0.3,
     lwd = 1.5,
     cex.axis = cexs.axis)
axis(
  2,
  las = 0,
  tcl = -0.3,
  lwd = 1.5,
  cex.axis = cexs.axis
)
box(lwd = 2)
dev.off()
#~~~~~~~~~~~~~~~

data$pred.env <- predict(out.env.lin.units)

pdf(
  paste0(
    figdir,
    "3-Residuals-by-Elevation-EnvModel.pdf"
  ),
  width = 5,
  height = 4
)
plot(
  data$Elevation,
  data$pred.env - data$RangeThru,
  xlim = elev.lims,
  xlab = "Elevation (m)",
  ylab = "Richness Residual (Predicted - Observed)",
  axes = F,
  type = 'n',
  cex.lab = cexs.lab
)

abline(h = 0)

points(
  data$Elevation,
  data$pred.env - data$RangeThru,
  pch = 21,
  cex = cexs.pt,
  bg = data$col
)

axis(1,
     tcl = -0.3,
     lwd = 1.5,
     cex.axis = cexs.axis)
axis(
  2,
  las = 0,
  tcl = -0.3,
  lwd = 1.5,
  cex.axis = cexs.axis
)
box(lwd = 2)

dev.off()

###################################################################
# Assessing Effecits of Co-linearity in Predictors PCA reanalysis
###################################################################

# PCA APPROACH

full_vars <- c("Width","Relief","Hypsometric.Integral","Roughness","mean_evi_above_threshold","TmaxAvg","PPTAvg","landfire_H_habitat_250")
geo_vars <- c("Width","Relief","Hypsometric.Integral","Roughness")
env_vars <- c("mean_evi_above_threshold","TmaxAvg","PPTAvg","landfire_H_habitat_250")

# Use complete cases so PC scores line up with rows of zdata
cc <- complete.cases(zdata[, c(full_vars, geo_vars, env_vars, "RangeThru", "Mountain")])
zd <- zdata[cc, ]

# PCA (scale. = TRUE is harmless if already z-scored)
pca_full <- prcomp(zd[,full_vars], center = TRUE, scale. = TRUE)
pca_geo <- prcomp(zd[, geo_vars], center = TRUE, scale. = TRUE)
pca_env <- prcomp(zd[, env_vars], center = TRUE, scale. = TRUE)

# Variance explained
summary(pca_full)
summary(pca_geo)
summary(pca_env)

# Loadings (needed to interpret the axes)
round(pca_full$rotation, 2)
round(pca_geo$rotation, 2)
round(pca_env$rotation, 2)

### Axis retention procedure
pdf(paste0(figdir,"3-ScreePlot-BrokenStick-Comparison.pdf"),
  width = 10,
  height = 6
)
par(mfrow = c(1, 3))
screeplot(pca_full, bstick = TRUE, main = "Full")
screeplot(pca_geo, bstick = TRUE, main = "Geo")
screeplot(pca_env, bstick = TRUE, main = "Env")
dev.off()
par(mfrow = c(1, 1))

# Retain axes whose eigenvalues exceed the broken-stick line, and check this against where the scree plot bends. Then add the scores to the data and refit. 

# Keep 2 predictors for full and 1 for geo and Env each.  
zd$fullPC1 <- pca_full$x[, 1]; zd$fullPC2 <- pca_full$x[, 2]
zd$geoPC1 <- pca_geo$x[, 1]
zd$envPC1 <- pca_env$x[, 1]

m_geo <- gam(RangeThru ~ Mountain + geoPC1, data = zd, method = "REML")
m_env <- gam(RangeThru ~ Mountain + envPC1, data = zd, method = "REML")
m_all <- gam(RangeThru ~ Mountain + fullPC1 + fullPC2, data = zd, method = "REML")

aic <- AIC(m_geo, m_env, m_all)

aicc <- AICctab(
  m_all,
  m_geo,
  m_env,
  base = TRUE,
  weights = TRUE
)

sink(paste0(resultsdir,'3-AIC(c)_PCA_2Full_1Env_1Geo.txt'))
print("AIC(c) COMPARISON - PCA - 2 axes for full - 1 axis for Env and Geo")
print("")
print('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~')
print("AIC")
aic
print("")
print('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~')
print("AICC")
aicc
sink()


# Keep 1 predictor for each model
zd$fullPC1 <- pca_full$x[, 1]
zd$geoPC1 <- pca_geo$x[, 1]
zd$envPC1 <- pca_env$x[, 1]

m_geo <- gam(RangeThru ~ Mountain + geoPC1, data = zd, method = "REML")
m_env <- gam(RangeThru ~ Mountain + envPC1, data = zd, method = "REML")
m_all <- gam(RangeThru ~ Mountain + fullPC1, data = zd, method = "REML")

aic <- AIC(m_geo, m_env, m_all)

aicc <- AICctab(
  m_all,
  m_geo,
  m_env,
  base = TRUE,
  weights = TRUE
)

sink(paste0(resultsdir,'3-AIC(c)_PCA_1Full_1Env_1Geo.txt'))
print("AIC(c) COMPARISON - PCA - 1 axis for all models")
print("")
print('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~')
print("AIC")
aic
print("")
print('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~')
print("AICC")
aicc
sink()

#### Variance Partitioning Analysis
v <- varpart(zd$RangeThru, ~geoPC1, ~envPC1, ~Mountain, data = zd)

sink(paste0(resultsdir,'3-PCA_VariancePartitioning.txt'))
print("Variance Partitioning Analysis - geoPC1, envPC1")
print("")
print('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~')
v
print("")
print('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~')
sink()
