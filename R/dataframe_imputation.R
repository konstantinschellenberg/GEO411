# Processing

library(raster)
library(tidyverse)
library(data.table)
library(ggplot2)

options(max.print = 50)

# set environment to "model"
setwd("D:/Geodaten/Master/projects/GEO411/model")



# LOAD DATA --------------------------------------------------------------------

# load dataframe
df50 = readRDS("dataframes/final_df.RDS")
df100 = readRDS("dataframes/final_df.RDS")

# IMPUTE -----------------------------------------------------------------------
# throw out all lines where there is one NA (not sure if this is the best way to impute data;)

dfs = list(df50 = df50, df100 = df100)

for (i in seq_along(dfs)){
    df = dfs[[i]]
    df = df[complete.cases(df),]

    # still 0s inside, gdalwarp: dstnodata = 0 !!

    names = c("chm",
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

    # rename colnames
    names(df) = names
    dfs[[i]] = df
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

ggplot(df, aes(height, bs_150821)) +
    geom_boxplot()

par(mfrow = c(3,3))
