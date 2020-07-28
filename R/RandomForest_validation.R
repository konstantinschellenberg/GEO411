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
library(MASS)
library(RColorBrewer)

#' Todo:
#' Write Accuracy measures to ggplot
#' Save lists savely

# LOAD DATA --------------------------------------------------------------------

trainingtestset = readRDS("model/dataframes/trainingtestset.RDS")
predictionset = readRDS("model/dataframes/predictionset.RDS")

# ONLY TRAINING AND PREDICTION -------------------------------------------------

outfile = lapply(c("50m", "100m", "200m"), function(x) paste0("Predicion_", x))

rf = vector("list", length = length(trainingtestset))
names(rf) = names(trainingtestset)

for (i in seq_along(trainingtestset)){
    cat("Loop ", i, ": Dataset --> ", names(trainingtestset)[i], sep = "")

    tts = trainingtestset[[i]]
    results = rf[[i]]

    # create task, mind the coordinates column and the projection
    task = TaskRegrST$new(id = "canopy_height", backend = tts, target = "chm",
                          coordinate_names = c("x", "y"),
                          crs = "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs")
    learner = lrn("regr.ranger", predict_type = "response")

    # RESAMPLE ---------------------------------------------------------------------

    future::plan("multiprocess")

    # setup resampling task
    resampling = rsmp("repeated-spcv-coords", folds = 5L, repeats = 2L)
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
    result = cbind(truth = pred$truth, response = pred$response) %>% as.data.frame()

    get_density <- function(x, y, ...) {
        dens <- MASS::kde2d(x, y, ...)
        ix <- findInterval(x, dens$x)
        iy <- findInterval(y, dens$y)
        ii <- cbind(ix, iy)
        return(dens$z[ii])
    }

    result = rf[[i]][[3]]

    result$density = get_density(result$truth, result$response, n = 100)

    myPalette = colorRampPalette(rev(brewer.pal(11, "Spectral")))
    sc = scale_colour_gradientn(colours = myPalette(100))

    library(Metrics)
    rmse = Metrics::rmse(result$truth, result$response)
    bias = Metrics::bias(result$truth, result$response)
    rsq = cor(result$truth, result$response) ^ 2
    n = length(result$truth)

    gg = ggplot(result, aes(truth, response)) +
        geom_abline() +
        geom_point(aes(color = density), alpha = alpha[i], shape = 16, size = 1) +
        geom_smooth(method = "lm") +
        annotate("text", label = paste("RMSE =", round(rmse, digits=3), "m", sep=" "), x=12,y=30, cex=4) +
        annotate("text", label = paste("Bias =", round(bias, digits=3), "", sep=" "), x=12,y=29,  cex=4) +
        annotate("text", label = paste("R² =", round(bias, digits=3), "", sep=" "), x=12,y=28, cex=4) +
        annotate("text", label = paste("n =", n, sep=" "), x=12,y=27, cex = 4) +
        sc +
        theme_classic() +
        theme(legend.position = "none") +
        coord_cartesian(xlim = c(10,30),
                        ylim = c(10,30))

    ggpath = paste0("model/plots/", outfile[i], "_density.png")
    ggsave(ggpath, gg, width = 6, height = 5)

    rf[[i]] = list(acc_rmp, acc_pred, result, gg)
    names(rf)[i] = names(trainingtestset)[i]
    # (end) No training, no prediction
}

# saveRDS(rf, "model/RF/batch_stats.RDS")

# Plot manually ----------------------------------------------------------------

rf = readRDS("model/RF/batch_stats.RDS")

# file size
print(paste(file.size("model/RF/batch_stats.RDS") / 1000000, "Mb"))

# define outfile
outfile = lapply(c("50m", "100m", "200m"), function(x) paste0("Predicion_", x))
alpha = c(1/20, 1/15, 1/5)

for (i in seq_along(trainingtestset)){
    cat("Loop ", i, ": Dataset --> ", names(trainingtestset)[i], sep = "")

    get_density <- function(x, y, ...) {
        dens <- MASS::kde2d(x, y, ...)
        ix <- findInterval(x, dens$x)
        iy <- findInterval(y, dens$y)
        ii <- cbind(ix, iy)
        return(dens$z[ii])
    }

    result = rf[[i]][[3]]

    result$density = get_density(result$truth, result$response, n = 100)

    myPalette = colorRampPalette(rev(brewer.pal(11, "Spectral")))
    sc = scale_colour_gradientn(colours = myPalette(100))

    library(Metrics)
    rmse = Metrics::rmse(result$truth, result$response)
    rmse = rf[[i]][[1]][[1]]
    bias = Metrics::bias(result$truth, result$response)
    bias = rf[[i]][[1]][[3]]
    rsq = cor(result$truth, result$response) ^ 2
    rsq = rf[[i]][[1]][[2]]
    n = length(result$truth)

    gg = ggplot(result, aes(truth, response)) +
        geom_abline() +
        geom_point(aes(color = density), alpha = alpha[i], shape = 16, size = 1) +
        geom_smooth(method = "lm") +
        annotate("text", label = paste("RMSE =", round(rmse, digits=3), "m", sep=" "), x=12,y=30, cex=4) +
        annotate("text", label = paste("Bias =", round(bias, digits=3), "", sep=" "), x=12,y=29,  cex=4) +
        annotate("text", label = paste("R² =", round(rsq, digits=3), "", sep=" "), x=12,y=28, cex=4) +
        annotate("text", label = paste("n =", n, sep=" "), x=12,y=27, cex = 4) +
        sc +
        theme_classic() +
        theme(legend.position = "none") +
        coord_cartesian(xlim = c(10,30),
                        ylim = c(10,30))

    ggpath = paste0("model/plots/", outfile[i], "_density.png")
    ggsave(ggpath, gg, width = 6, height = 5)
}
# (end)
