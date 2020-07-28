################
# GAMs for predicting forest Height from Backscatter and Coherence
################

# load libraries
library(mgcv)
library(raster)
library(dplyr)
library(ggplot2)

# CRS
crs = "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs"

# LOAD DATA --------------------------------------------------------------------
trainingtestset = readRDS("model/dataframes/trainingtestset.RDS")
predictionset = readRDS("model/dataframes/predictionset.RDS")

# Put data into gam
makefmls = function(trainingtestset){
    formulas = c()
    for (i in seq_along(trainingtestset)){
        df = trainingtestset[[i]]
        names=names(df)
        fml = c("chm ~")
        for (n in seq_along(names)){
            cov = names[i]
            if(!i == 1)
        }
    }
}


foms = makefmls(trainingtestset)
