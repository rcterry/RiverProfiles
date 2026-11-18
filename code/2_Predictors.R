#########################################################
## River Profile Project
## Generates plots of Predictor Variables along elevation
## Outputs min, max, and range of predictor variables
#########################################################

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PREDICTOR VARIABLES BY ELEVATION
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
elev.lims <- c(1700, 3000)
preds = data.frame(Elevation = seq(elev.lims[1], elev.lims[2], length = 100))

# Climate data
mod.Tmax.gam <- gam(TmaxAvg ~ s(Elevation), dat = dat)
mod.PPT.gam <- gam(PPTAvg ~ s(Elevation), dat = dat)

preds.mod.Tmax.gam <- predict(mod.Tmax.gam, preds, type = "link", se.fit =
                                T)
preds.PPT.gam <- predict(mod.PPT.gam, preds, type = "link", se.fit = T)

# Habitat data
mod.EVI.comb.gam <- gam(mean_evi_above_threshold ~ Elevation, dat = dat)
mod.HAB_LANDFIRE.gam <- gam(landfire_H_habitat_250 ~ Elevation, dat = dat)

preds.EVI.comb.gam <- predict(mod.EVI.comb.gam, preds, type = "link", se.fit =
                                T)
preds.HAB_LANDFIRE.gam <- predict(mod.HAB_LANDFIRE.gam,
                                  preds,
                                  type = "link",
                                  se.fit = T)

# Geo Data
mod.REL.gam <- gam(Relief ~ s(Elevation), dat = dat)
mod.HYP.gam <- gam(Hypsometric.Integral ~ s(Elevation), dat = dat)
mod.ROUGH.gam <- gam(Roughness ~ s(Elevation), dat = dat)
mod.WIDTH.gam <- gam(Width ~ s(Elevation), dat = dat)

preds.REL.gam <- predict(mod.REL.gam, preds, type = "link", se.fit = T)
preds.HYP.gam <- predict(mod.HYP.gam, preds, type = "link", se.fit = T)
preds.ROUGH.gam <- predict(mod.ROUGH.gam, preds, type = "link", se.fit =
                             T)
preds.WIDTH.gam <- predict(mod.WIDTH.gam, preds, type = "link", se.fit =
                             T)

# plotting

pdf(
  paste0(figdir, "2-ClimatePredictors-by-Elev-ForMS.pdf"),
  width = 5,
  height = 8,
  useDingbats = F
)
par(mgp = c(2, 0.5, 0))
par(mfrow = c(2, 1))


#~~~~~~~~~~~

plot(
  dat$Elevation,
  dat$TmaxAvg,
  xlim = elev.lims,
  xlab = "Elevation (m)",
  ylab = "Avg T max",
  axes = F,
  type = 'n',
  cex.lab = cexs.lab
)

polygon_x <- c(preds$Elevation, rev(preds$Elevation))
polygon_y <- c((preds.mod.Tmax.gam$fit - preds.mod.Tmax.gam$se.fit), rev((
  preds.mod.Tmax.gam$fit + preds.mod.Tmax.gam$se.fit
)))
polygon(polygon_x,
        polygon_y,
        col = "grey90",
        border = NA)

lines(
  preds$Elevation,
  preds.mod.Tmax.gam$fit,
  lwd = lwds[1],
  lty = ltys[1],
  col = cols[1]
)

points(
  dat$Elevation,
  dat$TmaxAvg,
  pch = 21,
  cex = cexs.pt,
  bg = dat$col
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

legend(
  'topright',
  legend = c('Toiyabes', 'Rubies'),
  pch = 21,
  pt.bg = pt.bgs,
  inset = 0.01,
  cex = 1
)

#~~~~~~~~

plot(
  dat$Elevation,
  dat$PPTAvg,
  xlim = elev.lims,
  xlab = "Elevation (m)",
  ylab = "Avg PPT",
  axes = F,
  type = 'n',
  cex.lab = cexs.lab
)

polygon_x <- c(preds$Elevation, rev(preds$Elevation))
polygon_y <- c((preds.PPT.gam$fit - preds.PPT.gam$se.fit), rev((preds.PPT.gam$fit +
                                                                  preds.PPT.gam$se.fit)))
polygon(polygon_x,
        polygon_y,
        col = "grey90",
        border = NA)

lines(
  preds$Elevation,
  preds.PPT.gam$fit,
  lwd = lwds[1],
  lty = ltys[1],
  col = cols[1]
)

points(
  dat$Elevation,
  dat$PPTAvg,
  pch = 21,
  cex = cexs.pt,
  bg = dat$col
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

#~~~~~~~~
#~~~~~~~~

pdf(
  paste0(figdir, "2-HabitatPredictors-by-Elev-ForMS.pdf"),
  width = 5,
  height = 8,
  useDingbats = F
)
par(mgp = c(2, 0.5, 0))
par(mfrow = c(2, 1))

#~~ EVI
plot(
  dat$Elevation,
  dat$mean_evi_above_threshold,
  xlim = elev.lims,
  ylim = c(0.1, 0.5),
  xlab = "Elevation (m)",
  ylab = "MODIS Terra EVI Growing Season Avg",
  axes = F,
  type = 'n',
  cex.lab = cexs.lab
)

polygon_x <- c(preds$Elevation, rev(preds$Elevation))
polygon_y <- c((preds.EVI.comb.gam$fit - preds.EVI.comb.gam$se.fit), rev((
  preds.EVI.comb.gam$fit + preds.EVI.comb.gam$se.fit
)))
polygon(polygon_x,
        polygon_y,
        col = "grey90",
        border = NA)

lines(
  preds$Elev,
  preds.EVI.comb.gam$fit,
  lwd = lwds[1],
  lty = ltys[1],
  col = cols[1]
)

points(
  dat$Elevation,
  dat$mean_evi_above_threshold,
  pch = 21,
  cex = cexs.pt,
  bg = dat$col
)

legend(
  'topleft',
  legend = c('Toiyabes', 'Rubies'),
  pch = 21,
  pt.bg = pt.bgs,
  inset = 0.01,
  cex = 1.5
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

#~~~ Habitat

plot(
  dat$Elevation,
  dat$landfire_H_habitat_250,
  xlim = elev.lims,
  xlab = "Elevation (m)",
  ylab = "GAP/LANDFIRE Shannon Diversity",
  axes = F,
  type = 'n',
  cex.lab = cexs.lab
)

polygon_x <- c(preds$Elevation, rev(preds$Elevation))
polygon_y <- c((preds.HAB_LANDFIRE.gam$fit - preds.HAB_LANDFIRE.gam$se.fit),
               rev((
                 preds.HAB_LANDFIRE.gam$fit + preds.HAB_LANDFIRE.gam$se.fit
               )))
polygon(polygon_x,
        polygon_y,
        col = "grey90",
        border = NA)

lines(
  preds$Elev,
  preds.HAB_LANDFIRE.gam$fit,
  lwd = lwds[1],
  lty = ltys[1],
  col = cols[1]
)

points(
  dat$Elevation,
  dat$landfire_H_habitat_250,
  pch = 21,
  cex = cexs.pt,
  bg = dat$col
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

#~~~ Geology

pdf(
  paste0(figdir, "2-GeologyPredictors-by-Elev-ForMS-1.pdf"),
  width = 5,
  height = 8,
  useDingbats = F
)
par(mgp = c(2, 0.5, 0))
par(mfrow = c(2, 1))

plot(
  dat$Elevation,
  dat$Relief,
  xlim = elev.lims,
  xlab = "Elevation (m)",
  ylab = "Relief",
  axes = F,
  type = 'n',
  cex.lab = cexs.lab
)

polygon_x <- c(preds$Elevation, rev(preds$Elevation))
polygon_y <- c((preds.REL.gam$fit - preds.REL.gam$se.fit), rev((preds.REL.gam$fit +
                                                                  preds.REL.gam$se.fit)))
polygon(polygon_x,
        polygon_y,
        col = "grey90",
        border = NA)

lines(
  preds$Elev,
  preds.REL.gam$fit,
  lwd = lwds[1],
  lty = ltys[1],
  col = cols[1]
)

points(
  dat$Elevation,
  dat$Relief,
  pch = 21,
  cex = cexs.pt,
  bg = dat$col
)

legend(
  'bottomright',
  legend = c('Toiyabes', 'Rubies'),
  pch = 21,
  pt.bg = pt.bgs,
  inset = 0.01,
  cex = 1
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

#~~~~~~~~

plot(
  dat$Elevation,
  dat$Width,
  xlim = elev.lims,
  xlab = "Elevation (m)",
  ylab = "Canyon Width",
  axes = F,
  type = 'n',
  cex.lab = cexs.lab
)

polygon_x <- c(preds$Elevation, rev(preds$Elevation))
polygon_y <- c((preds.WIDTH.gam$fit - preds.WIDTH.gam$se.fit), rev((
  preds.WIDTH.gam$fit + preds.WIDTH.gam$se.fit
)))
polygon(polygon_x,
        polygon_y,
        col = "grey90",
        border = NA)

lines(
  preds$Elev,
  preds.WIDTH.gam$fit,
  lwd = lwds[1],
  lty = ltys[1],
  col = cols[1]
)

points(
  dat$Elevation,
  dat$Width,
  pch = 21,
  cex = cexs.pt,
  bg = dat$col
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

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pdf(
  paste0(figdir, "2-GeologyPredictors-by-Elev-ForMS-2.pdf"),
  width = 5,
  height = 8,
  useDingbats = F
)
par(mgp = c(2, 0.5, 0))
par(mfrow = c(2, 1))

plot(
  dat$Elevation,
  dat$Hypsometric.Integral,
  xlim = elev.lims,
  xlab = "Elevation (m)",
  ylab = "Hypsometric Integral",
  axes = F,
  type = 'n',
  cex.lab = cexs.lab
)

polygon_x <- c(preds$Elevation, rev(preds$Elevation))
polygon_y <- c((preds.HYP.gam$fit - preds.HYP.gam$se.fit), rev((preds.HYP.gam$fit +
                                                                  preds.HYP.gam$se.fit)))
polygon(polygon_x,
        polygon_y,
        col = "grey90",
        border = NA)

lines(
  preds$Elev,
  preds.HYP.gam$fit,
  lwd = lwds[1],
  lty = ltys[1],
  col = cols[1]
)

points(
  dat$Elevation,
  dat$Hypsometric.Integral,
  pch = 21,
  cex = cexs.pt,
  bg = dat$col
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


#~~~~~~~~

plot(
  dat$Elevation,
  dat$Roughness,
  xlim = elev.lims,
  xlab = "Elevation (m)",
  ylab = "Roughness",
  axes = F,
  type = 'n',
  cex.lab = cexs.lab
)

polygon_x <- c(preds$Elevation, rev(preds$Elevation))
polygon_y <- c((preds.ROUGH.gam$fit - preds.ROUGH.gam$se.fit), rev((
  preds.ROUGH.gam$fit + preds.ROUGH.gam$se.fit
)))
polygon(polygon_x,
        polygon_y,
        col = "grey90",
        border = NA)

lines(
  preds$Elev,
  preds.ROUGH.gam$fit,
  lwd = lwds[1],
  lty = ltys[1],
  col = cols[1]
)

points(
  dat$Elevation,
  dat$Roughness,
  pch = 21,
  cex = cexs.pt,
  bg = dat$col
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
#~~~~~~~~
#~~~~~~~~~~~~~~~~~~

# Summary table: min, max, and range of each predictor by mountain
predictor_vars <- c(
  'TmaxAvg',
  'PPTAvg',
  'landfire_H_habitat_250',
  'mean_evi_above_threshold',
  'Relief',
  'Width',
  'Hypsometric.Integral',
  'Roughness'
)

summary_by_mountain <- do.call(rbind, lapply(split(dat, dat$Mountain), function(sub) {
  stats <- lapply(predictor_vars, function(v) {
    mn <- min(sub[[v]], na.rm = TRUE)
    mx <- max(sub[[v]], na.rm = TRUE)
    data.frame(
      Mountain = sub$Mountain[1],
      Variable = v,
      Min      = mn,
      Max      = mx,
      Spread   = mx - mn
    )
  })
  do.call(rbind, stats)
}))
rownames(summary_by_mountain) <- NULL

# Separate data frame: difference in spread (Toiyabe - Ruby) for each predictor
spread_diff <- data.frame(
  Variable           = predictor_vars,
  Toiyabe_minus_Ruby_Spread = sapply(predictor_vars, function(v) {
    ruby_spread    <- diff(range(dat[[v]][dat$Mountain == "Ruby"], na.rm = TRUE))
    toiyabe_spread <- diff(range(dat[[v]][dat$Mountain == "Toiyabe"], na.rm = TRUE))
    toiyabe_spread - ruby_spread
  }),
  row.names = NULL
)

write.csv(
  summary_by_mountain,
  paste0(resultsdir, '2-PredictorSummary-by-Mountain.csv'),
  row.names = F
)
write.csv(
  spread_diff,
  paste0(resultsdir, '2-PredictorSpread-Toiyabe-minus-Ruby.csv'),
  row.names = F
)

#######################################################
## Representing Co-linearity in Predictors
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PAIRWISE CORRELATIONS

# Pairwise Correlation Plot
corrs <- cor(dat[,c(
  'TmaxAvg',
  'PPTAvg',
  'landfire_H_habitat_250',
  'mean_evi_above_threshold',
  'Relief',
  'Width',
  'Hypsometric.Integral',
  'Roughness')])

pdf(
  paste0(figdir, "2-CorrelationPlot-forMS.pdf"),
  width = 12,
  height = 8,
  useDingbats = F
)
corrplot.mixed(corrs,
               upper = 'ellipse',
               tl.pos = 'lt'
)
dev.off()

#################################################################
# Calculate means of within group and between group correlations

# Rename cols and rows with variable type
nms <- c("Env","Env","Env","Env","Geo","Geo","Geo","Geo")
colnames(corrs) <- nms
row.names(corrs) <- nms

# Only use half the matrix
corrs[upper.tri(corrs, diag = TRUE)] <- NA

EE <- corrs[which(row.names(corrs)=="Env"),which(colnames(corrs)=="Env")]
GG <- corrs[which(row.names(corrs)=="Geo"),which(colnames(corrs)=="Geo")]
EG <- corrs[-which(row.names(corrs)=="Env"),which(colnames(corrs)=="Env")]

# Calculate mean correlations (using absolute values)
EEcor <- mean(abs(EE),na.rm=T)
GGcor <- mean(abs(GG),na.rm=T)
EGcor <- mean(abs(EG),na.rm=T)

corr.stats <- cbind(c("EE","GG","EG"),
                    c(mean(abs(EE),na.rm=T), mean(abs(GG),na.rm=T), mean(abs(EG),na.rm=T)),
                    c(range(abs(EE),na.rm=T)[1], range(abs(GG),na.rm=T)[1], range(abs(EG),na.rm=T)[1]),
                    c(range(abs(EE),na.rm=T)[2], range(abs(GG),na.rm=T)[2], range(abs(EG),na.rm=T)[2])
)
colnames(corr.stats) <- c("Comparison","Mean","Min","Max")
corr.stats <- data.frame(corr.stats)
for (c in 2:ncol(corr.stats)) {corr.stats[,c] <- as.numeric(corr.stats[,c])}

## Table for MS
write.csv(
  corr.stats, 
  paste0(resultsdir, '2-PredictorCorrelationStats.csv'),
  row.names = F
  )

#################################################################
# Calculate Variance Inflation Factors (VIFs)

lm.geo <- lm(RangeThru ~ 
               Mountain + 
               Width + 
               Relief + 
               Hypsometric.Integral + 
               Roughness, 
             data = dat)
lm.env <- lm(RangeThru ~ 
               Mountain + 
               mean_evi_above_threshold + 
               TmaxAvg + 
               PPTAvg + 
               landfire_H_habitat_250, 
             data = dat)
lm.full <- lm(RangeThru ~ 
                Mountain + 
                Width + 
                Relief + 
                Hypsometric.Integral + 
                Roughness + 
                mean_evi_above_threshold +
                TmaxAvg + 
                PPTAvg + 
                landfire_H_habitat_250, 
              data = dat)

f <- vif(lm.full)             
g <- vif(lm.geo)
e <- vif(lm.env)

write.csv(
  corr.stats, 
  paste0(resultsdir, '2-PredictorCorrelationStats.csv'),
  row.names = F
)

sink(paste0(resultsdir, '2-VarianceInflationFactors.txt'))
print("VARIANCE INFLATION FACTORS")
print("")
print('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~')
print("FULL MODEL")
f
print("")
print('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~')
print("GEO ONLY")
g
print("")
print('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~')
print("ENV ONLY")
e
sink()


