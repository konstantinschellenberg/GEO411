# load libraries
library(raster)
library(dplyr)
library(mgcv)
library(sperrorest)
library(scattermore)

options(max.print = 200, scipen = 100)

# set environment to "model"
env = "D:/Geodaten/Master/projects/GEO411/"
setwd(env)


# LOADING DATASETS -------------------------------------------------------------

trainsets = readRDS("model/dataframes/trainsets.RDS")
testsets = readRDS("model/dataframes/testsets.RDS")

# MULTILINEAR FIT --------------------------------------------------------------

attach(trainsets[[1]])
lm.fit = lm(chm ~ bs_150821 + bs_151002 + bs_160219 + bs_180525 + coh_15_15 + coh_15_16 + coh_15_16)


# PREDICTION -------------------------------------------------------------------

right = predict(lm.fit, testsets[[1]][,2:8]) %>%
    cbind(testsets[[1]][10:11]) # get coordinates back/not really necessary here

left = dfs[[1]] %>% select("chm", "x", "y")

validation = left_join(left, right, by = c("x", "y"))
plot(prediction$chm, prediction$.)

# ------------------------------------------------------------------------------

# make scatterplot of chm agains all backscatters
# set up plotting grid
par(mfrow=c(1,1))
p = n2mfrow(length(df_vars)-2)
par(mfrow = c(p[[1]], p[[2]]))
# plot all scatterplots
# scattermore <3
for (i in 1:length(df_vars)){
    if(i < length(df_vars)-2){
        scattermoreplot(df_vars[[1]], df_vars[[i+1]], xlim = c(0,50), ylim = c(0,3), main=paste("CHM x ", names(df_vars)[[i+1]]))}
}
