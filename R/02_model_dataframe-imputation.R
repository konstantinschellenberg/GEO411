#' GEO411 Modelling Preparation (2/2)
#' Script to create
#' 1. Modelling dataset
#' 1.1 Training sample (70%)
#' 1.2 Validation sample (30%)
#' 2. Prediction dataset, without dependet variable CHM
#'
#' All datasets are stored under /model/dataframes/ in the git repo (folder within gitignore)
#'
#' Init: 04.07.2020, Konstantin Schellenberg

library(raster)
library(tidyverse)
library(data.table)
library(ggplot2)
library(sf)
library(sperrorest)

options(max.print = 200, scipen = 100)

# set environment to "model"
env = "D:/Geodaten/Master/projects/GEO411/"
setwd(env)

# run this to remove the files created in this script
files = c("model/dataframes/trainsets.RDS", "model/dataframes/testsets.RDS", "model/dataframes/testsets.RDS", "model/dataframes/predsets.RDS")
sapply(files, function(x) file.remove(x))

# LOAD DATASETS ----------------------------------------------------------------

dfs = readRDS("model/dataframes/final_df.RDS")

##########################
# 1. MODELLING DATASET
##########################

# IMPUTE -----------------------------------------------------------------------
# throw out all lines where there is one NA (not sure if this is the best way to impute data;)

for (i in seq_along(dfs)){
    df = dfs[[i]]
    df = df[complete.cases(df),]

    # still 0s inside, gdalwarp: dstnodata = 0 !!
    dfs[[i]] = df
    cat("NAs removed from the dataset")
}

# dataset with removed NAs
if (!file.exists("model/dataframes/datasets.RDS")){
    saveRDS(dfs, "model/dataframes/datasets.RDS")
}

# 1.1 Training Sample ----------------------------------------------------------
# 1.2 Validation Sample --------------------------------------------------------
# Sperrorest to be implemented


trainsets = vector("list", 2)
testsets = vector("list", 2)

set.seed(100)

for (i in seq_along(dfs)){
    df = dfs[[i]]

    # sample 70% of the data
    train_set = sample(nrow(df), 0.7 * nrow(df))
    train_set = df[train_set, ]
    # get the remaining 30% of the data
    test_set = setdiff(seq_len(nrow(df)), train_set)
    test_set = df[test_set, ]

    trainsets[[i]] = train_set
    testsets[[i]] = test_set
    cat("Train and Validation Dataset created")
}

if (!file.exists("model/dataframes/trainsets.RDS")){
    saveRDS(trainsets, "model/dataframes/trainsets.RDS")
}
if (!file.exists("model/dataframes/testsets.RDS")){
    saveRDS(testsets, "model/dataframes/testsets.RDS")
}

##########################
# 2. PREDICTION DATASET
##########################

# read in original dfs again
dfs = readRDS("model/dataframes/final_df.RDS")

predsets = vector("list", 2)

for (i in seq_along(dfs)){
    df = dfs[[i]] %>% select(-chm)
    predsets[[i]] = df
    cat("Prediction Dataset created")
}

if (!file.exists("model/dataframes/predsets.RDS")){
    saveRDS(predsets, "model/dataframes/predsets.RDS")
}

# CATEGORISATION ---------------------------------------------------------------
#' Aim is to assign incrementing categories of CH for boxplot of variable relationship

for (i in seq_along(dfs)){
    df = dfs[[i]]
    df = dfs[[1]] %>% as.data.table()

    breaks = seq(floor(min(df[,chm])), floor(max(df[,chm])))
    labels = as.character(breaks)
    labels = labels[1:length(labels) - 1] # remove last entry

    df[, height:= cut(chm, breaks = breaks, labels = labels)]

    # df[, table(height)]
    # df %>% select(chm, height) %>% arrange(height)
    dfs[[i]] = df
}

# BOXPLOT ----------------------------------------------------------------------

# get one dataframe
df = dfs[[1]]

# backscatter
ggplot(df, aes(height, bs_150821)) +
    geom_boxplot()

# coherences
ggplot(df, aes(height, coh_15_15)) +
    geom_boxplot()
ggplot(df, aes(height, coh_15_16)) +
    geom_boxplot()
ggplot(df, aes(height, coh_18_18)) +
    geom_boxplot()


par(mfrow = c(3,3))


# SPERROREST VIGNETTE ----------------------------------------------------------

data("maipo", package = "sperrorest")
predictors <- colnames(maipo)[5:ncol(maipo)]
# Construct a formula:
fo <- as.formula(paste("croptype ~", paste(predictors, collapse = "+")))
library("ranger")
fit <- ranger(fo, data = maipo)
fit
pred <- predict(fit, data = maipo, type = "response")
mean(pred$predictions != maipo$croptype)
table(pred = pred$predictions, obs = maipo$croptype)

rf_predfun <- function(object, newdata, fac = NULL) {

    library(nnet)
    majority <- function(x) {
        levels(x)[which.is.max(table(x))]
    }

    majority_filter <- function(x, fac) {
        for (lev in levels(fac)) {
            x[fac == lev] <- majority(x[fac == lev])
        }
        x
    }

    pred <- predict(object, data = newdata)
    if (!is.null(fac)) pred <- majority_filter(pred$predictions, newdata[, fac])
    return(pred)
}

res_rf_sp <- sperrorest(fo,
                        data = maipo, coords = c("utmx", "utmy"),
                        model_fun = ranger,
                        pred_fun = rf_predfun,
                        pred_args = list(fac = "field"),
                        smp_fun = partition_factor_cv,
                        smp_args = list(
                            fac = "field",
                            repetition = 1:10, nfold = 5
                        ),
                        benchmark = TRUE, progress = 2
)
lapply(res_rf_sp$error_rep, summary)
summary(res_rf_sp$error_rep$test_accuracy)
