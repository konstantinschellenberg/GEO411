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

# DATA -------------------------------------------------------------------------

trainingtestset = readRDS("model/dataframes/trainingtestset.RDS")
predictionset = readRDS("model/dataframes/predictionset.RDS")

# vorerst nur res_50 einladen
# input = trainingtestset[["df50"]]
# predictionset = predictionset[["df50"]]
input = trainingtestset[["df100"]]
predictionset = predictionset[["df100"]]

# CREATE TASK ------------------------------------------------------------------
mlr_reflections$task_types # task types available
task = TaskRegrST$new(id = "canopy_height", backend = input, target = "chm",
                                  coordinate_names = c("x", "y"),
                                  crs = "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs")

# inspect task -----------------------------------------------------------------

task$backend$colnames
task$backend

task$col_roles
task$feature_types

task$task_type
task$coordinate_names

mlr_reflections$task_col_roles$regr # var roles for classification

# CREATE LEARNER ---------------------------------------------------------------

learner = lrn("regr.ranger", predict_type = "response")
learner$param_set$ids()

# set built-in filter & hyperparameters
learner$param_set$values = list(num.trees = 500L, mtry = 1L, importance = "impurity")

################################################################################

# RESAMPLE ---------------------------------------------------------------------

library(future)
future::plan("multiprocess")

# available resampling methods:
mlr_resamplings

# setup resampling task
resampling = rsmp("repeated-spcv-coords", folds = 10L, repeats = 5L)

# SpCV sperrorest
# autoplot(resampling, task)

resampling$instantiate(task)
resampling$iters
resampling

# splitting task in train and test
str(resampling$train_set(1))
str(resampling$test_set(1))

# run: TIME INTENSIVE
# Fehler in unserialize(node$con) :
#     Failed to retrieve the value of MultisessionFuture (future_lapply-3) from cluster SOCKnode #3 (PID 92 on localhost ‘localhost’). The reason reported was ‘Lesefehler aus Verbindung’. Post-mortem diagnostic: No process exists with this PID, i.e. the localhost worker is no longer alive.
rr = mlr3::resample(task, learner, resampling, store_models = TRUE)

# save result:
c = 0
if (c == 1) {
    saveRDS(rr, "model/RF/ResamplingResult50.rds")
}

rr = readRDS("model/RF/ResamplingResult50.rds")

# results
mlr_measures
rr$aggregate(measures = msr("regr.rmse"))
rr$aggregate(measures = msr("time_train"))
rr$score(msr("regr.rmse"))

# plotting resampling results
# spatial cross-validation Brenning et al.


# PREDICT ON TEST --------------------------------------------------------------

pred_test = rr$prediction()

### results
mlr_measures
pred_test$score(msr("regr.rmse"))
pred_test$score(msr("regr.bias"))
pred_test$score(msr("regr.rsq"))

autoplot(pred_test)

# Accuracy
acc = cbind(truth = pred_test$truth, response = pred_test$response) %>% as.data.frame()

ggplot(acc, aes(truth, response)) +
    geom_abline() +
    geom_point(alpha = 1/50, shape = 16, color = "black", size = 2) +
    theme_classic() +
    coord_cartesian(xlim = c(0,40),
                    ylim = c(0,40))


# TRAIN ------------------------------------------------------------------------

learner$train(task)

# save model:
# saveRDS(learner, "model/RF/model.RDS")
readRDS()

# PREDICT ----------------------------------------------------------------------

newdata = predictionset[complete.cases(predictionset), ] %>% as.data.table()

sum(is.na(newdata))
nrow(newdata)
nrow(input)
colnames(newdata)
colnames(input)

prediction = learner$predict_newdata(task = task, newdata = newdata)

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

out = p.rasterise(prediction, newdata)

# Quickplot
plot(out)

# EXPORT --------------------------------------------------------------
writeRaster(out, filename = "model/RF/prediction100m",
            format="GTiff", datatype='FLT4S', overwrite=TRUE, na.rm=TRUE)
