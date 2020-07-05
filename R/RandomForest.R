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

# DATA -------------------------------------------------------------------------

trainingtestset = readRDS("model/dataframes/trainingtestset.RDS")
predictionset = readRDS("model/dataframes/predictionset.RDS")

# vorerst nur res_50 einladen
input = trainingtestset[["df50"]]
predictionset = predictionset[["df50"]]
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
learner$param_set$values = list(num.trees =500L, mtry = 1, importance = "impurity")


# optional: FILTERING ----------------------------------------------------------

# local({
#
#     filter = flt("importance", learner = learner)
#     filter$calculate(task)
#
#
#     head(as.data.table(filter), 50)
#     tail(as.data.table(filter), 50)
#
#     a = as.data.table(filter)
#     b = substr(a$feature, start = 1,stop = 3) %>%
#         as.factor()
#
#     plot(x = a$score, pch = 16, col = as.factor(b), main = "Predicting layers in RF Model",
#          xlab = "layer", ylab = "score")
# })


# TUNING seperately ------------------------------------------------------------

hyperparameters = function(){
    library("paradox")
    tune = ParamSet$new(list(
        ParamInt$new("num.trees", lower = 1, upper = 500),
        ParamInt$new("mtry", lower = 1, upper = 4)
    ))

    measures = msr("regr.rmse")
    terminator = term("evals", n_evals = 10)
    tuner = tnr("grid_search", resolution = 5)
    resampling = rsmp("repeated-spcv-coords", folds = 3L, repeats = 5L)

    instance = TuningInstance$new(
        task = task,
        learner = learner,
        resampling = resampling,
        measures = measures,
        param_set = tune,
        terminator = terminator
    )

    result = tuner$tune(instance)
    saveRDS(instance, "model/RF/instance.RDS")


    return(instance)
}

# run
# instance = hyperparameters()
instance = readRDS("model/RF/instance.RDS")
# results:
paste(
cat("best performance for is: ", instance$result$perf, "m RMSE\n"),
cat("With the following hyperparameter options:\n\n"),
print(instance[["result"]][["params"]])
)

# num.trees =500L, mtry = 1 --> ideal for our dataset

instance$archive(unnest = "params")[, c("num.trees", "mtry")]

# TUNING automatically ---------------------------------------------------------

hyperparameters2 = function(){
    library("paradox")
    tune = ParamSet$new(list(
        ParamInt$new("num.trees", lower = 1, upper = 500),
        ParamInt$new("mtry", lower = 1, upper = 4)
    ))
    measures = msr("regr.rmse")
    terminator = term("evals", n_evals = 10)
    tuner = tnr("grid_search", resolution = 5)
    resampling = rsmp("repeated-spcv-coords", folds = 3L, repeats = 5L)
    at = AutoTuner$new(
        learner = learner,
        resampling = resampling,
        measures = measures,
        tune_ps = tune,
        terminator = terminator,
        tuner = tuner
    )
    return(at)
}

at = hyperparameters2() %>% print()

# RESAMPLE ---------------------------------------------------------------------

library(future)
future::plan("cluster")

# available resampling methods:
mlr_resamplings

# setup resampling task
resampling = rsmp("repeated-spcv-coords", folds = 2L, repeats = 2L)

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
c = 1
if (c == 1) {
    saveRDS(rr, "model/RF/ResamplingResult50.rds")
}

rr = readRDS("model/RF/ResamplingResult50.rds")

### results
mlr_measures
rr$aggregate(measures = msr("rmse"))
rr$aggregate()
rr$score(msr("regr.rmse"))

### plotting resampling results
# spatial cross-validation Brenning et al.
autoplot(resampling, task)

# metrics
autoplot(rr)
autoplot(rr, type = "histogram", bins = 30L)

# PREDICT ON TEST --------------------------------------------------------------

pred_test = rr$prediction()
head(fortify(pred_test))
as.data.table(pred_test)

a = pred_test$confusion
print(a)
write_rds(a, path = paste0(path_developement, "confusion_rn.rda"))
pred_test$prob
pred_test$response

### results
pred_test$score(msr("classif.acc"))
autoplot(pred_test)

# TRAIN ------------------------------------------------------------------------

learner$train(task)

# save model:
# saveRDS(learner, "model/RF/model.RDS")
readRDS()

# PREDICT ----------------------------------------------------------------------

#' * `predict_newdata(newdata, task = NULL)`\cr
#'   (`data.frame()`, [Task]) -> [Prediction]\cr
#'   Uses the model fitted during `$train()` in to create a new [Prediction] based on the new data in `newdata`.
#'   Object `task` is the task used during `$train()` and required for conversions of `newdata`.
#'   If the learner's `$train()` method has been called, there is a (size reduced) version of the training task stored in the learner.
#'   If the learner has been fitted via [resample()] or [benchmark()], you need to pass the corresponding task stored
#'   in the [ResampleResult] or [BenchmarkResult], respectively.

# Piped version, easier:
# pred = learner$train(task)$predict_newdata(newdata = newdata.split1)
# ------------------------------------------------------------------------------

newdata = predictionset[complete.cases(predictionset), ] %>% as.data.table()

sum(is.na(newdata))
nrow(newdata)
nrow(input)
colnames(newdata)
colnames(input)

pred = learner$predict_newdata(task = task, newdata = newdata)

pred$predict_types
pred$response

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

out = stack(out2) #%>% trim()

# Quickplot
plot(out)

### Stats on the prediction
autoplot(pred)
head(as.data.table(pred))

# EXPORT -----------------------------------------------------------------------

# define output function
exporting = function(output, input){

    # bind coords on data.table
    out4 = cbind(output, x = input$x, y = input$y)

    # make sf coords
    out3 = st_as_sf(out4, coords = c("x", "y"))

    # set crs
    st_crs(out3) = 32632

    # to sp for gridding, functionality is not yet found in sf... st_rasterize may work in `stars`
    out2 = as(out3, "Spatial")

    # gridding
    gridded(out2) = TRUE
    class(out2)

    out = stack(out2) #%>% trim()
    return(out)

}

# convert results to raster*
# raster_prediction = exporting(pred, input)



# SAVE PREDICTION --------------------------------------------------------------
writeRaster(out, filename = "model/RF/prediction100m",
            format="GTiff", datatype='FLT4S', overwrite=TRUE, na.rm=TRUE)
