#' GEO411 Modelling Plotting
#'
#' Categorisation of the data
#'
#' Init: 04.07.2020, Konstantin Schellenberg

library(raster)
library(tidyverse)
library(data.table)
library(ggplot2)
library(sf)
library(grid)
library(gridExtra)

options(max.print = 200, scipen = 100)

# set environment to "model"
env = "D:/Geodaten/Master/projects/GEO411/"
setwd(env)

# LOAD DATASETS ----------------------------------------------------------------

master = readRDS("model/dataframes/trainingtestset.RDS")
dfs = master

# CATEGORISATION ---------------------------------------------------------------
#' Aim is to assign incrementing categories of CH for boxplot of variable relationship

for (i in seq_along(dfs)){
    df = dfs[[i]] %>% as.data.table()

    breaks = seq(floor(min(df[,chm])), floor(max(df[,chm])))
    labels = as.character(breaks)
    labels = labels[1:length(labels) - 1] # remove last entry

    df[, height:= cut(chm, breaks = breaks, labels = labels)]

    # df[, table(height)]
    # df %>% select(chm, height) %>% arrange(height)
    dfs[[i]] = df
}

# BOXPLOT ----------------------------------------------------------------------

theme_set(theme_classic())

for (i in seq_along(dfs)){
    df = dfs[[i]]
    # coherences
    g1 = ggplot(df, aes(height, coh_15_15, fill = "green")) +
        geom_boxplot()
    g2 = ggplot(df, aes(height, coh_15_16)) +
        geom_boxplot()
    g3 = ggplot(df, aes(height, coh_18_18)) +
        geom_boxplot()
    plots = list(g1, g2, g3)
    arr1 = grid.arrange(grobs = plots, nrow = 3, top = "top",
                 bottom = grid::grid.text("gridtext", gp = gpar(fontface = 3, fontsize = 9, hjust = 1, x = 1)))

   h1 = ggplot(df, aes(height, bs_150821)) +
        geom_boxplot(aes(fill = "red")) +
        stat_boxplot(geom ='errorbar', width = 0.6)

    h2 = ggplot(df, aes(height, bs_151002)) +
        geom_boxplot()
    h3 = ggplot(df, aes(height, bs_160219)) +
        geom_boxplot()
    h4 = ggplot(df, aes(height, bs_180525)) +
        geom_boxplot()
    h5 = ggplot(df, aes(height, bs_180720)) +
        geom_boxplot()
    plots = list(h1, h2, h3, h4, h5)
    grid.arrange(grobs = plots, nrow = 3, top = "top",
                 bottom = grid::grid.text("gridtext", gp = gpar(fontface = 3, fontsize = 9, hjust = 1, x = 1)))


    c# saveRDS()
}

# backscatter
ggplot(df, aes(height, bs_150821)) +
    geom_boxplot()




par(mfrow = c(3,3))

# DEPRECATED -------------------------------------------------------------------
# trainsets = vector("list", 2)
# testsets = vector("list", 2)
#
# set.seed(100)
#
# for (i in seq_along(dfs)){
#     df = dfs[[i]]
#
#     # sample 70% of the data
#     train_set = sample(nrow(df), 0.7 * nrow(df))
#     train_set = df[train_set, ]
#     # get the remaining 30% of the data
#     test_set = setdiff(seq_len(nrow(df)), train_set)
#     test_set = df[test_set, ]
#
#     trainsets[[i]] = train_set
#     testsets[[i]] = test_set
#     cat("Train and Validation Dataset created")
# }
#
# if (!file.exists("model/dataframes/trainsets.RDS")){
#     saveRDS(trainsets, "model/dataframes/trainsets.RDS")
# }
# if (!file.exists("model/dataframes/testsets.RDS")){
#     saveRDS(testsets, "model/dataframes/testsets.RDS")
# }
