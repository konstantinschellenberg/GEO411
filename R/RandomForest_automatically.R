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
library(tidyverse)

# OUTPUT FUNCTION --------------------------------------------------------------

p.rasterise = function(prediction, newdata){
    output = cbind(response = prediction$response, x = newdata$x, y = newdata$y)
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
    #rm(list=ls())
}


################
##  Transect  ##
################
# load test_chm
test_chm_path_max = "/home/robin/geodata/geo411/GEO411_FSH_Roda/ancillary_data/chm_max_resampled.tif"
test_chm_path_med = "/home/robin/geodata/geo411/GEO411_FSH_Roda/ancillary_data/chm_med_resampled.tif"
test_chm_path_average = "/home/robin/geodata/geo411/GEO411_FSH_Roda/ancillary_data/chm_average_resampled.tif"
test_chm_50_max = raster(test_chm_path_max)
test_chm_50_med = raster(test_chm_path_med)
test_chm_50_average = raster(test_chm_path_average)


# load them back in
prediction_paths = list.files("model/RF", full.names = T)
resolutions = c("50m", "100m", "200m")
predictions = vector("list", length = length(resolutions))
# put them in a list
predictions = map(prediction_paths, ~ raster(.x)) %>% `names<-`(., resolutions)
# crop 50m predictions to test_chm
cropped_50_prediction = crop(predictions[[3]], test_chm_50_max)
# draw a line and extract the 50m
plot(cropped_50_prediction)
#line = raster::drawLine()
# load it back in
line = st_read("/home/robin/geodata/geo411/GEO411_FSH_Roda/transect.gpkg")
plot(line, add = T)

# extract "truth" values
max_truth = raster::extract(test_chm_50_max, line, cellnumbers=T) %>% as.data.frame(.)
# extract median truth
med_truth = raster::extract(test_chm_50_med, line, cellnumbers=T) %>% as.data.frame(.)
# extract average truth
average_truth = raster::extract(test_chm_50_average, line, cellnumbers=T) %>% as.data.frame(.)
# extract only 50m prediction values
pred_50m_extract = raster::extract(cropped_50_prediction, line, cellnumbers=T) %>% as.data.frame(.)

# create one dataframe for the prediction values and its coordainates
coords_pred = coordinates(cropped_50_prediction)[pred_50m_extract[,1], ]
df_pred = cbind(pred_50m_extract, coords_pred)

# add the value-column from the extraction from the other rasters
final_df = cbind(df_pred, max_truth[,2], med_truth[,2], average_truth[,2])
names(final_df) = c("cell", "pred", "x", "y", "max", "med", "avg")
final_df = final_df %>% select(-c(cell)) %>% select(pred, max, med, avg, x, y)

# make it a clean dataset
final_df_clean = final_df %>% pivot_longer(cols = c("pred", "max", "med", "avg"), names_to="ResampleMethod")

# plot it
# take out prediction
final_df0 = final_df_clean %>% filter(ResampleMethod!="pred")
final_df1 = final_df_clean %>% filter(ResampleMethod=="pred")
g = ggplot(final_df0) + geom_line(aes(x = y, y = value, color = ResampleMethod), alpha=.4) + ylab("Height [m]") +
    xlab("UTM zone 32N (Y-Koordinate)") + ggtitle("Out-of-sample transect of Prediction Heights vs. Lidar Heights") + theme_minimal()# + scale_color_hue(h=c(0,360),l=10, c=80)
g + geom_line(data=final_df1, aes(x=y, y=value, color=ResampleMethod), size=1.3) + theme_minimal()
