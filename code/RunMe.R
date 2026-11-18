#########################################################################
#########################################################################
#########################################################################

rm(list=ls())
options(stringsAsFactors=F)

#########################################################################

library(mgcv)
library(bbmle)
library(corrplot)
library(car)
library(vegan)
library(ape)
library (geosphere)

#########################################################################

datadir <- '../data/'
resultsdir <- '../results/'
figdir <- '../results/figs/'

datadir <- '/Users/Becca/Git/RiverProfiles/data/'
resultsdir <- '/Users/Becca/Git/RiverProfiles/results/'
figdir <- '/Users/Becca/Git/RiverProfiles/results/figs/'

#########################################################################

cols <-  c('grey20','grey50')
cols2 <- c('sienna2','dodgerblue2')
pt.bgs <- cols2
ltys <- c(2, 3)
lwds <- c(2, 2)
cexs.lab <- 1.2
cexs.axis <- 1
cexs.pt <- 1.5

#########################################################################

source('1_Range_Richness.R')
source('2_Predictors.R')
source('3_Regressions.R')
source('4_Autocorrelation.R')

#########################################################################
#########################################################################
#########################################################################