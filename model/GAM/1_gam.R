################
# GAMs for predicting forest Height from Backscatter and Coherence
################

# load data
dfs = readRDS("model/dataframes/final_df.RDS")
datasets = readRDS("model/dataframes/datasets.RDS")
trainsets = readRDS("model/dataframes/trainsets.RDS")
testsets = readRDS("model/dataframes/testsets.RDS")
predsets = readRDS("model/dataframes/predsets.RDS")


# check dimensions of dataframes for whole extent
sapply(dfs[[1]], length)
# dimension of dataframe for intersecting extent without NAs
sapply(datasets[[1]], length)
# check number of NAs for entire extent --> Lots of NA for CHM as it got resampled to dummy-raster
# which is the one of the 8 intersecting covariates
sapply(dfs[[1]], function(x) sum(is.na(x)))
# check number of NAs --> here 0
sapply(datasets[[1]], function(x) sum(is.na(x)))


#######################
##  CHM ~ Coherence  ##
#######################

# 50m

# select only coherence from datasets dataframe
coherence_df =
