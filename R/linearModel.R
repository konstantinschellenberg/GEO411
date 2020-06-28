# load libraries
library(raster)
library(dplyr)
library(mgcv)
library(sperrorest)
library(scattermore)

# load dataframe
df_vars = readRDS("dev/final_df.RDS")
# throw out all lines where there is one NA (not sure if this is the best way to impute data;)
df_vars = df_vars[complete.cases(df_vars),]
# give shorter names?
names(df_vars) = c("chm",
                   "bs_150821",
                   "bs_151002",
                   "bs_160219",
                   "bs_180525",
                   "bs_180720",
                   "coh_15_15",
                   "coh_15_16",
                   "coh_18_18",
                   "x",
                   "y")


# check class of each predictor
struc = lapply(df_vars, class)
struc.df = as.data.frame(do.call(rbind, struc))
struc.df

# make scatterplot of chm agains all backscatters
# set up plotting grid
par(mfrow=c(1,1))
p = n2mfrow(length(df_vars)-2)
par(mfrow = c(p[[1]], p[[2]]))
# plot all scatterplots
# scattermore <3
for (i in 1:length(df_vars)){
    if(i < length(df_vars)-2){
    scattermoreplot(df_vars[[1]], df_vars[[i+1]], xlim = c(0,50), ylim = c(0,3), main=paste("CHM x ", names(df_vars)[[i+1]]))}
}

# make model
attach(df_vars)
# aiiaiai
lm0 = lm(chm ~ bs_150821 + bs_151002 + bs_160219 + bs_180525 + bs_180720)
