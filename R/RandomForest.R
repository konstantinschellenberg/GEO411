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

# DATA -------------------------------------------------------------------------

dfs = readRDS("model/dataframes/final_df.RDS")
datasets = readRDS("model/dataframes/datasets.RDS")
trainsets = readRDS("model/dataframes/trainsets.RDS")
testsets = readRDS("model/dataframes/testsets.RDS")
predsets = readRDS("model/dataframes/predsets.RDS")

# vorerst nur res_50 einladen
input = datasets[["df50"]]

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
learner$param_set$values = list(num.trees =500L, mtry = 4, importance = "impurity")


# optional: FILTERING ----------------------------------------------------------

local({

    filter = flt("importance", learner = learner)
    filter$calculate(task)


    head(as.data.table(filter), 50)
    tail(as.data.table(filter), 50)

    a = as.data.table(filter)
    b = substr(a$feature, start = 1,stop = 3) %>%
        as.factor()

    plot(x = a$score, pch = 16, col = as.factor(b), main = "Predicting layers in RF Model",
         xlab = "layer", ylab = "score")
})


# TUNING seperately ------------------------------------------------------------

library("paradox")
tune = ParamSet$new(list(
    ParamInt$new("num.trees", lower = 1, upper = 500),
    ParamInt$new("mtry", lower = 1, upper = 4)
))

measures = msr("regr.rmse")
terminator = term("evals", n_evals = 20)
tuner = tnr("grid_search", resolution = 5)
resampling = rsmp("repeated-spcv-coords", folds = 6L, repeats = 2L)

instance = TuningInstance$new(
    task = task,
    learner = learner,
    resampling = resampling,
    measures = measures,
    param_set = tune,
    terminator = terminator
)

result = tuner$tune(instance)

# results:
instance$archive(unnest = "params")[, c("num.trees", "mtry", "terminator")]

# TUNING automatically ---------------------------------------------------------

at = AutoTuner$new(
    learner = learner,
    resampling = resampling,
    measures = measures,
    tune_ps = tune,
    terminator = terminator,
    tuner = tuner
)
at$train(task)

saveRDS(at, paste0(path_rds, "automatedTunedLearner_eval20.rds"))

at = readRDS(paste0(path_rds, "automatedTunedLearner_eval20.rds"))

at$predict(task)

# RESAMPLE ---------------------------------------------------------------------

future::plan("multiprocess") # anmerkung: future::plan turns over order of logging in the resample algorithm! -> Patrick S sagen

as.data.table(mlr_resamplings) # error -> report Patrick S
mlr_resamplings

# setup resampling task
resampling = rsmp("repeated-spcv-coords", folds = 6L, repeats = 10L)

resampling$instantiate(task)
resampling$iters
resampling

# splitting task in train and test
str(resampling$train_set(1))
str(resampling$test_set(1))

# run: TIME INTENSIVE
rr = mlr3::resample(task, learner, resampling, store_models = TRUE)

# save result:
c = 1
if (c == 1) {
    write_rds(rr, paste0(path_developement, "rf_acc/ResamplingResult_v.rds"))
    rr = readRDS(paste0(path_developement, "rf_acc/ResamplingResult_rn.rds"))
}

### results
mlr_measures
rr$aggregate(measures = msr("classif.acc"))
rr$aggregate()
rr$score(msr("classif.acc"))

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
print(learner$model)

# save model:
saveRDS(learner, paste0(path_rds, "Learner.rds"))

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


pred = learner$predict_newdata(task = task, newdata = newdata)

pred$confusion
pred$prob
pred$response


### Stats on the prediction
autoplot(pred)
head(as.data.table(pred))

# EXPORT -----------------------------------------------------------------------

output = data.table::as.data.table(pred)

exporting(output = output, input = newdata.split2, filepath = paste0(path_prediction, "02-12_rightbottom_split1_red_nir_vh"))
