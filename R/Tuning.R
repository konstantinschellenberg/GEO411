# Tuning


task = x


# optional: FILTERING ----------------------------------------------------------

filter = flt("importance", learner = learner)
filter$calculate(task)

head(as.data.table(filter), 10)

table = as.data.table(filter)
table$feature = table$feature %>%
    as.factor() %>%
    factor(levels = table$feature[order(table$score)])

ggplot(table) +
    geom_point(aes(feature, score, size = score)) +
    theme_classic()



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
rr = instance$best()

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
