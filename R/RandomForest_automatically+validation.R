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
library(future)

#' Todo:
#' Write Accuracy measures to ggplot
#' Save lists savely

# LOAD DATA --------------------------------------------------------------------

trainingtestset = readRDS("model/dataframes/trainingtestset.RDS")
predictionset = readRDS("model/dataframes/predictionset.RDS")

# ONLY TRAINING AND PREDICTION -------------------------------------------------

outfile = lapply(c("50m", "100m", "200m"), function(x) paste0("Predicion_", x))

rf = vector("list", length = length(n))
names(rf) = names(trainingtestset)

for (i in seq_along(trainingtestset)){
    cat("Loop ", i, ": Dataset --> ", names(trainingtestset)[i], sep = "")

    tts = trainingtestset[[i]]
    ps = predictionset[[i]]
    results = rf[[i]]

    # create task, mind the coordinates column and the projection
    task = TaskRegrST$new(id = "canopy_height", backend = tts, target = "chm",
                          coordinate_names = c("x", "y"),
                          crs = "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs")
    learner = lrn("regr.ranger", predict_type = "response")

    # RESAMPLE ---------------------------------------------------------------------

    future::plan("multiprocess")

    # setup resampling task
    resampling = rsmp("repeated-spcv-coords", folds = 5L, repeats = 4L)
    resampling$instantiate(task)
    cat("Number of iterations:", resampling$iters)
    cat("Type of Resampling:", resampling$man)

    # resampling
    rr = mlr3::resample(task, learner, resampling, store_models = TRUE)

    # results
    acc_rmp = list(
        rmse = rr$aggregate(measures = msr("regr.rmse")),
        rsq = rr$aggregate(measures = msr("regr.rsq")),
        bias = rr$aggregate(measures = msr("regr.bias"))
    )
    # PREDICT ON TEST --------------------------------------------------------------

    pred = rr$prediction()

    ### results
    acc_pred = list(
        rmse = pred$score(measures = msr("regr.rmse")),
        rsq = pred$score(measures = msr("regr.rsq")),
        bias = pred$score(measures = msr("regr.bias"))
    )

    # Accuracy
    result = cbind(truth = pred_test$truth, response = pred_test$response) %>% as.data.frame()

    gg = ggplot(result[100,], aes(truth, response)) +
        geom_abline() +
        geom_point(alpha = 1/50, shape = 16, color = "black", size = 2) +
        theme_classic() +
        coord_cartesian(xlim = c(0,40),
                        ylim = c(0,40))


    rf[[i]] = list(acc_rmp, acc_pred, results, gg)
    names(rf)[i] = names(trainingtestset)[i]
    # (end) No training, no prediction
}

saveRDS(rf, "model/RF/batch_stats.RDS")
