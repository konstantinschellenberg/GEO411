# Multiple linear regression
###########################

# load libraries
library(raster)
library(dplyr)
library(sf)
library(mlr3)
library(sperrorest)
library(scattermore)

########################
##  Data Preparation  ##
########################

# load data
datasets = readRDS("model/dataframes/trainingtestset.RDS")
predsets = readRDS("model/dataframes/predictionset.RDS")

# check dimensions of dataframes for whole extent
sapply(dfs[[1]], length)
# dimension of dataframe for intersecting extent without NAs
sapply(datasets[[1]], length)
# check number of NAs for entire extent --> Lots of NA for CHM as it got resampled to dummy-raster
# which is the one of the 8 intersecting covariates
sapply(dfs[[1]], function(x) sum(is.na(x)))
# check number of NAs --> here 0
sapply(datasets[[1]], function(x) sum(is.na(x)))

###########
##  EDA  ##
###########

# 50m
df50 = datasets$df50
# make scatterplot of chm agains all backscatters
# set up plotting grid
par(mfrow=c(1,1))
p = n2mfrow(length(df50)-2)
par(mfrow = c(p[[1]], p[[2]]))
# plot all scatterplots
# scattermore
for (i in 1:length(df50)){
    if(i < length(df50)-2){
        scattermoreplot(df50[[1]], df50[[i+1]], xlim = c(0,50), ylim = c(0,1), main=paste("CHM x ", names(df50)[[i+1]]))}
}

# 100m
df100 = datasets$df100
# make scatterplot of chm agains all backscatters
# set up plotting grid
par(mfrow=c(1,1))
p = n2mfrow(length(df100)-2)
par(mfrow = c(p[[1]], p[[2]]))
# plot all scatterplots
# scattermore
for (i in 1:length(df100)){
    if(i < length(df100)-2){
        scattermoreplot(df100[[1]], df100[[i+1]], xlim = c(0,50), ylim = c(0,1), main=paste("CHM x ", names(df100)[[i+1]]))}
}

##################################
##  Multiple Linear Regression  ##
##################################
# first only for 50m
attach(datasets[[1]])
# stepweise forward selection
# Based on the AIC
fit_null_model = lm(chm ~ 1)
fit_forward = step(fit_null_model, scope = chm ~ bs_150821+bs_151002+
                       bs_160219+bs_180525+bs_180720+coh_15_15+
                       coh_15_16+coh_18_18, direction = "forward", trace=2)

# predict it on dataset
df50_raster = predsets$df50
new_order = names(df50_raster) %>% grep("x|y", ., invert = F)
df50_raster_new = df50_raster[-new_order]
df50_raster_new = cbind(df50_raster[new_order], df50_raster_new)

# make raster from dataframe
raster_list = vector(mode="list", length = 8)
for(i in seq_along(df50_raster_new)){
    if(i != 1 & i!= 2){
        print(i-2)
        print(names(df50_raster_new)[[i]])
        r = rasterFromXYZ(df50_raster_new[c(1,2,i)], res = c(50,50), crs="+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs")
        raster_list[[i-2]] = r

    }
}

# stack the covariates
s = stack(raster_list)
p = raster::predict(s, fit_forward)

