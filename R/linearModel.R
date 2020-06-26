#######################
# Create linear model to predict forest heigth given the radar backscatter
#######################
# (2/2) Modelling

library(caret)
library(tidyverse)
library(raster)

# set working directory
setwd("/home/robin/geodata/geo411/GEO411_FSH_Roda/")
setwd("D:/Geodaten/GEO411/01_data/")

# load the pre-processed dataframe
df = readRDS("dev/final_df.RDS")

attach(df)
topophase.cor.geo_32632_20x20
