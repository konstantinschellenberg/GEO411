#' RandomForest Regression of InSAR data for Canopy Height estimation
#' init: 04.07.2020, Konstantin Schellenberg
#'

options(max.print = 200, scipen = 100)

# set environment to "model"
env = "D:/Geodaten/Master/projects/GEO411/"
setwd(env)

library(mlr3)
library(mlr3spatiotempcv)
library(mlr3learners)
library(mlr3filters)
library(mlr3tuning)
library(tidyverse)
library(sf)
library(raster)
library(ggplot2)

# OUTPUT FUNCTION --------------------------------------------------------------

p.rasterise = function(prediction, newdata){
    output = cbind(response = pred$response, x = newdata$x, y = newdata$y)
    out4 = output %>% as.data.table()
    # make sf coords
    out3 = st_as_sf(out4, coords = c("x", "y"))
    # set crs
    st_crs(out3) = 32632
    # to sp for gridding, functionality is not yet found in sf... st_rasterize may work in `stars`
    out2 = as(out3, "Spatial")
    # gridding
    gridded(out2) = TRUE
    out = raster(out2) #%>% trim()
    return(out)
}

# LOAD DATA --------------------------------------------------------------------

trainingtestset = readRDS("model/dataframes/trainingtestset.RDS")
predictionset = readRDS("model/dataframes/predictionset.RDS")

# ONLY TRAINING AND PREDICTION -------------------------------------------------

outfile = sapply(c("50m", "100m", "200m"), function(x) paste0("Predicion_", x))

for (i in seq_along(trainingtestset)){
    cat("Loop ", i, ": Dataset --> ", names(trainingtestset)[i], sep = "")

    tts = trainingtestset[[i]]
    ps = predictionset[[i]]
    newdata = ps[complete.cases(ps), ] %>% as.data.table()


    # create task, mind the coordinates column and the projection
    task = TaskRegrST$new(id = "canopy_height", backend = tts, target = "chm",
                          coordinate_names = c("x", "y"),
                          crs = "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs")
    learner = lrn("regr.ranger", predict_type = "response")
    learner$param_set$values = list(num.trees = 500L, mtry = 1L, importance = "impurity")

    # train learner
    learner$train(task)

    # predict on new data
    prediction = learner$predict_newdata(task = task, newdata = newdata)

    # transform to raster
    out = p.rasterise(prediction, newdata)

    # write out
    outpath = paste0("model/RF/", outfile[i])
    writeRaster(out, filename = outpath, format="GTiff", datatype='FLT4S', overwrite=TRUE, na.rm=TRUE)

    # clean env
    rm(list=ls())
}
