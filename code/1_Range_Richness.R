######################################################
## River Profile Project
## Generates plots of Ranges along elevation, and richness by elevation
########

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ELEVATIONAL RANGES
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Set WD and read in data
data <- read.csv(paste0(datadir, "Species_Ranges.csv"), header = T)

# Sort data by min elevation
data <- data[order(data$Min, data$Max), ]
data$plot <- seq(1, nrow(data))

# plotting
data$col <- NA
data$col[which(data$Mountain == "Toiyabe")] <- "sienna2"
data$col[which(data$Mountain == "Rubies")] <- "dodgerblue2"

elev.lims <- c(1700, 3000)

cexs.lab = 1.2
cexs.axis = 1
cexs.pt = 1.5

#~~~ Rubies Only ~~~~~~~~
data.r <- data[which(data$Mountain == "Rubies"), ]
data.r$plot <- seq(1, nrow(data.r))

pdf(
  paste0(figdir, "1-RangeExtents-by-Elev_Plot-Rubies.pdf"),
  width = 5,
  height = 4,
  useDingbats = F
)
par(mgp = c(2, 0.5, 0))

plot(
  1,
  1,
  xlim = elev.lims,
  ylim = c(1, nrow(data.r)),
  type = "n",
  axes = F,
  xlab = "Elevation (m)",
  ylab = "",
  cex.lab = cexs.lab
)
arrows(
  data.r$Min,
  data.r$plot,
  data.r$Max,
  data.r$plot,
  length = 0,
  lwd = 4,
  col = "dodgerblue2"
)
axis(1,
     tcl = -0.3,
     lwd = 1.5,
     cex.axis = cexs.axis)
axis(
  2,
  labels = data.r$Species,
  at = data.r$plot,
  las = 1,
  tcl = 0,
  lwd = 1.5,
  cex.axis = 0.75
)
legend("topleft", legend = "Rubies", x.intersp = 0)
box(lwd = 2)

dev.off()

#~~~~~ Toiyabes Only ~~~~~~~~~~~~

data.t <- data[which(data$Mountain == "Toiyabe"), ]
data.t$plot <- seq(1, nrow(data.t))

pdf(
  paste0(figdir, "1-RangeExtents-by-Elev_Plot-Toiyabes.pdf"),
  width = 5,
  height = 4,
  useDingbats = F
)
par(mgp = c(2, 0.5, 0))

plot(
  1,
  1,
  xlim = elev.lims,
  ylim = c(1, nrow(data.t)),
  type = "n",
  axes = F,
  xlab = "Elevation (m)",
  ylab = "",
  cex.lab = cexs.lab
)
arrows(
  data.t$Min,
  data.t$plot,
  data.t$Max,
  data.t$plot,
  length = 0,
  lwd = 4,
  col = "sienna2"
)
axis(1,
     tcl = -0.3,
     lwd = 1.5,
     cex.axis = cexs.axis)
axis(
  2,
  labels = data.t$Species,
  at = data.t$plot,
  las = 1,
  tcl = 0,
  lwd = 2,
  cex.axis = 0.75
)
legend("topleft", legend = "Toiyabes", x.intersp = 0)
box(lwd = 2)

dev.off()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# RICHNESS ESTIMATE BY ELEVATION
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# read in mammal data
dat <- read.csv(paste0(datadir, "Data_All_Compiled.csv"), header = T)
dat$col <- NA
dat$col[which(dat$Mountain == "Ruby")] <- "dodgerblue2"
dat$col[which(dat$Mountain == "Toiyabe")] <- "sienna2"

# fit gam functions to range-thru richness
preds = data.frame(Elevation = seq(elev.lims[1], elev.lims[2], length = 100))

mod.rrich.gam <- gam(RangeThru ~ s(Elevation), dat = dat)

preds$rrich.gam <- predict(mod.rrich.gam, preds, type = "link", se.fit =
                             T)

#~~~~~~~~~~~~

cols =  c('grey20', 'grey50')
pt.cols = c('sienna2', 'dodgerblue2')
ltys = c(2, 3)
lwds = c(2, 2)
cexs.lab = 1.2
cexs.axis = 1
cexs.pt = 1.5

pdf(
  paste0(figdir, "1-RangeRich-byElevation-ForMS.pdf"),
  width = 5,
  height = 4
)
par(mgp = c(2, 0.5, 0))

#~~~~~~~~~~~ Detection Corrected Range Thru Richness Estimates
plot(
  1,
  1,
  xlim = elev.lims,
  ylim = c(4, 15),
  xlab = "Elevation (m)",
  ylab = "Range through richness",
  axes = F,
  type = 'n',
  cex.lab = cexs.lab
)

polygon_x <- c(preds$Elevation, rev(preds$Elevation))
polygon_y <- c((preds$rrich.gam$fit - preds$rrich.gam$se.fit), rev((
  preds$rrich.gam$fit + preds$rrich.gam$se.fit
)))
polygon(polygon_x,
        polygon_y,
        col = "grey90",
        border = NA)

lines(
  preds$Elevation,
  preds$rrich.gam$fit,
  lwd = lwds[1],
  lty = ltys[1],
  col = cols[1]
)

points(
  dat$Elevation,
  dat$RangeThru,
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
  las = 1,
  tcl = -0.3,
  lwd = 1.5,
  cex.axis = cexs.axis
)
box(lwd = 2)

legend(
  'topleft',
  legend = c('Toiyabes', 'Rubies'),
  pch = 21,
  pt.bg = c('sienna2', 'dodgerblue2'),
  inset = 0.01,
  cex = 1
)

dev.off()

