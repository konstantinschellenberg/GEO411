#' Scatterplot of lidar canopy height and InSAR coherence and
#' init: 24.05.2020

library(raster)
library(ggplot2)
library(tidyverse)
library(gridExtra)
library(grid)

# the plot functions are stored here
source("R/plot_functions.R")

# user input:

# mother directory, as specified by robin
directory = "D:/Geodaten/GEO411/01_data"
plot_dir = "D:/Geodaten/GEO411/07_plots/"
lidar = "D:/Geodaten/GEO411/01_data/ancillary_data/chm_maskedtofnf_30_32632_-99nodata.tif"
targetresolution = "20"

setwd("D:/Geodaten/Master/projects/GEO411")

# Preprocess -------------------------------------------------------------------

require("optparse", attach.required = T)

# if needed, run the coherence preprocessing steps in preprocess_coherence.R
file = "D:/Geodaten/Master/projects/GEO411/R/preprocess_coherence.R"
system(command = sprintf("Rscript %s --help", file)) # run help

cmd = sprintf("Rscript %s --directory=%s --lidar=%s --targetresolution=%s", file, directory, lidar, targetresolution)
print(cmd)

# run pre-processing
# system(cmd)
system("gdalinfo")

# Plotting ---------------------------------------------------------------------

# fetch data
l = list.files(path = directory, full.names = T, recursive = T)
# get data
pre_processed_coherences = l[grepl("topophase.cor.geo.proj.resamp.na.crop.tolidar.tif$", l)]
pre_processed_backscatter = l[grepl("Cal_TC_32632_20x20_crop.tolidar.tif$", l)]
pre_processed_backscatter.db = l[grepl("Cal_TC_32632_20x20_crop.tolidar.db.tif$", l)]

# Quickplot
# for (i in seq_along(pre_processed_coherences)) plot(raster(pre_processed_coherences[[i]]))
# for (i in seq_along(pre_processed_backscatter)) plot(raster(pre_processed_backscatter[[i]]))
# for (i in seq_along(pre_processed_backscatter.db)) plot(raster(pre_processed_backscatter.db[[i]]))

# variable names
spl = strsplit(pre_processed_coherences, .Platform$file.sep)
coh_names = paste0(sapply(spl, function(x){x[[length(x)-2]]}), "_ALOS2_coherence")

spl = strsplit(pre_processed_backscatter, .Platform$file.sep)
bsc_names = paste0(sapply(spl, function(x){
    name = x[[length(x)]]
    substr(name, start = 40, stop = 45) %>% paste0("20", .)
}), "_ALOS2_backscatter")

spl = strsplit(pre_processed_backscatter.db, .Platform$file.sep)
bsc.db_names = paste0(sapply(spl, function(x){
    name = x[[length(x)]]
    substr(name, start = 40, stop = 45) %>% paste0("20", .)
}), "_ALOS2_backscatter.db")


# load data
coh = brick(stack(pre_processed_coherences))
names(coh) = coh_names
bsc = brick(stack(pre_processed_backscatter))
names(bsc) = bsc_names
bsc.db = brick(stack(pre_processed_backscatter.db))
names(bsc.db) = bsc.db_names
chm = raster(lidar)
names(chm) = "Lidar"

# Histogram --------------------------------------------------------------------
# Histogram creation for CHM
h = hist(chm, breaks = 50, maxpixels = 500000)
h$counts = h$counts * 400 / 10000
h_data = data.frame(breaks = h$mids, counts = h$counts)

# hist plotting
g1 = ggplot(h_data, aes(x = breaks, y = counts)) +
    geom_bar(stat = "identity", fill='darkgreen', alpha = 0.8, width = .5) +
    ggtitle("Lidar Canopy Height Distribution in Roda Catchment") +
    xlab("Canopy Height [m]")+
    ylab("Area [ha]") +
    theme_classic()

ggsave(plot = g1, filename = "Canopy Height Histogram.svg", path = plot_dir, device = "svg", width = 5, height = 3)

# Histogram for Coherence
hist(coh, breaks = 50, maxpixels = 500000)
# Histogram for Backscatter
hist(bsc, breaks = 50, maxpixels = 500000)
# Histogram for Backscatter DB
hist(bsc.db, breaks = 50, maxpixels = 500000)

# Scatterplot ------------------------------------------------------------------
# Creating cross scatterplots for coherence and CHM for 1 date

# if needed, resample chm to coherence
coh = resample(coh, chm)
bsc = resample(bsc, chm)
bsc.db = resample(bsc.db, chm)
# matching extents? Otherwise run preprocessing again -> proprocess_coherence.R
identical(extent(chm), extent(coh))
identical(extent(chm), extent(bsc))
identical(extent(chm), extent(bsc.db))

#################################
# COHERENCE
#################################

# basic plotting
# plot(values(coh[[1]]), values(chm))

arrange_plot = function(stack, chm, names, xlab, ylab, toptitle, nrow, gridtext){

    plots = vector("list", nlayers(stack))
    for (i in 1:nlayers(stack)){
        plots[[i]] = scatterplot(chm, stack[[i]], bins = 100,
                                 xlab = xlab, ylab = ylab, title  = names)
    }

    grid.arrange(grobs = plots, nrow = nrow, top = toptitle,
                 bottom = grid::grid.text(gridtext, gp = gpar(fontface = 3, fontsize = 9, hjust = 1, x = 1)))
}

arrange_plot(coh, chm, coh_names, xlab = "Canopy Height [m]", ylab = "Coherence", toptitle = "ALOS-2 HV Coherence",
             nrow = 3, gridtext = "Interferograms processed by ISCE") %>%
    ggsave(filename = "Coherence_HV-CHM.svg", path = plot_dir, device = "svg", limitsize = F, width = 3, height = 9)

arrange_plot(bsc.db, chm, bsc.db_names, xlab = "Canopy Height [m]", ylab = "Backscatter coefficient [db]", toptitle = "ALOS-2 HV Backscatter",
             nrow = 3, gridtext = "Processed by SNAP") %>%
    ggsave(filename = "Backscatter_HV-CHM.svg", path = plot_dir, device = "svg", limitsize = F, width = 7, height = 9)
