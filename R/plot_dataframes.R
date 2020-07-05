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

plot_dir = "model/plots/"
theme_set(theme_classic())
theme(legend.position = "none")

text = list(
    res50 = list(main = list(coh = c("Coherence - Canopy Height Resolution 50 m"), bsc = c("Backscatter - Canopy Height Resolution 50 m")),
                 filename = c("CHM-COH_50m", "CHM-BSC_50m")),
    res100 = list(main = list(coh = c("Coherence - Canopy Height Resolution 100 m"), bsc = c("Backscatter - Canopy Height Resolution 100 m")),
                  filename = c("CHM-COH_100m", "CHM-BSC_100m"))
)

for (i in seq_along(dfs)){

    df = dfs[[i]]

    # define color ramp for coherences
    colfunc = colorRampPalette(c("white", "darkgreen"))

    # coherences
    g1 = ggplot(df, aes(height, coh_15_15)) +
        geom_boxplot(aes(fill = df$height), outlier.size = 0.5) +
        scale_x_discrete(breaks = seq(5, 50, 5)) +
        scale_y_continuous(limits = c(0,1)) +
        scale_fill_discrete(type = colfunc(max(as.numeric(df$height), na.rm = T))) +
        ylab("Coherence 2015-08-21/2015-10-02") +
        theme(legend.position = "none")
    g2 = ggplot(df, aes(height, coh_15_16, fill = "green")) +
        geom_boxplot(aes(fill = df$height), outlier.size = 0.5) +
        scale_x_discrete(breaks = seq(5, 50, 5)) +
        scale_y_continuous(limits = c(0,1)) +
        scale_fill_discrete(type = colfunc(max(as.numeric(df$height), na.rm = T))) +
        ylab("Coherence 2015-10-02/2016-02-19") +
        theme(legend.position = "none")
    g3 = ggplot(df, aes(height, coh_18_18, fill = "green")) +
        geom_boxplot(aes(fill = df$height), outlier.size = 0.5) +
        scale_x_discrete(breaks = seq(5, 50, 5)) +
        scale_y_continuous(limits = c(0,1)) +
        scale_fill_discrete(type = colfunc(max(as.numeric(df$height), na.rm = T))) +
        ylab("Coherence 2018-05-25/2018-07-20") +
        theme(legend.position = "none")
    plots = list(g1, g2, g3)
    grid.arrange(grobs = plots, nrow = 3, top = text[[i]]$main$coh,
                 bottom = grid::grid.text("", gp = gpar(fontface = 3, fontsize = 9, hjust = 1, x = 1))) %>%
        ggplot2::ggsave(filename = paste0(text[[i]]$filename[1], ".svg"), path = plot_dir, device = "svg", limitsize = F, width = 4, height = 10)

    # new colorramp for backscatter
    # colfunc = colorRampPalette(c("white", "#556B2F")) # Olivgrün
    colfunc = colorRampPalette(c("white", "red"))

    h1 = ggplot(df, aes(height, bs_150821)) +
        geom_boxplot(aes(fill = df$height), outlier.size = 0.5) +
        stat_boxplot(geom ='errorbar', width = 0.6) +
        scale_x_discrete(breaks = seq(5, 50, 5)) +
        scale_y_continuous(limits = c(0, 0.2)) +
        scale_fill_discrete(type = colfunc(max(as.numeric(df$height), na.rm = T))) +
        ylab("Backscatter σ0 linear 2015-08-21") +
        theme(legend.position = "none")
    h2 = ggplot(df, aes(height, bs_151002)) +
        geom_boxplot(aes(fill = df$height), outlier.size = 0.5) +
        stat_boxplot(geom ='errorbar', width = 0.6) +
        scale_x_discrete(breaks = seq(5, 50, 5)) +
        scale_y_continuous(limits = c(0, 0.2)) +
        scale_fill_discrete(type = colfunc(max(as.numeric(df$height), na.rm = T))) +
        ylab("Backscatter σ0 linear 2015-10-02") +
        theme(legend.position = "none")
    h3 = ggplot(df, aes(height, bs_160219)) +
        geom_boxplot(aes(fill = df$height), outlier.size = 0.5) +
        stat_boxplot(geom ='errorbar', width = 0.6) +
        scale_x_discrete(breaks = seq(5, 50, 5)) +
        scale_y_continuous(limits = c(0, 0.2)) +
        scale_fill_discrete(type = colfunc(max(as.numeric(df$height), na.rm = T))) +
        ylab("Backscatter σ0 linear 2016-02-19") +
        theme(legend.position = "none")
    h4 = ggplot(df, aes(height, bs_180525)) +
        geom_boxplot(aes(fill = df$height), outlier.size = 0.5) +
        stat_boxplot(geom ='errorbar', width = 0.6) +
        scale_x_discrete(breaks = seq(5, 50, 5)) +
        scale_y_continuous(limits = c(0, 0.2)) +
        scale_fill_discrete(type = colfunc(max(as.numeric(df$height), na.rm = T))) +
        ylab("Backscatter σ0 linear 2018-05-25") +
        theme(legend.position = "none")
    h5 = ggplot(df, aes(height, bs_180720)) +
        geom_boxplot(aes(fill = df$height), outlier.size = 0.5) +
        stat_boxplot(geom ='errorbar', width = 0.6) +
        scale_x_discrete(breaks = seq(5, 50, 5)) +
        scale_y_continuous(limits = c(0, 0.2)) +
        scale_fill_discrete(type = colfunc(max(as.numeric(df$height), na.rm = T))) +
        ylab("Backscatter σ0 linear 2018-07-20") +
        theme(legend.position = "none")
    plots = list(h1, h2, h3, h4, h5)
    grid.arrange(grobs = plots, nrow = 3, top = text[[i]]$main$bsc,
                 bottom = grid::grid.text("", gp = gpar(fontface = 3, fontsize = 9, hjust = 1, x = 1))) %>%
        ggplot2::ggsave(filename = paste0(text[[i]]$filename[2], ".svg"), path = plot_dir, device = "svg", limitsize = F, width = 8, height = 10)
}
