#' Scatterplot of lidar canopy height and InSAR coherence and
#' init: 24.05.2020

# user input:

directory = "D:/Geodaten/GEO411/01_data"
plot_dir = "D:/Geodaten/GEO411/07_plots/"
lidar = "D:/Geodaten/GEO411/01_data/chm_maskedtofnf_30_32632_-99nodata.tif"
targetresolution = "30"

# Preprocess -------------------------------------------------------------------

require("optparse", attach.required = T)

# if needed, run the coherence preprocessing steps in preprocess_coherence.R
file = "D:/Geodaten/Master/projects/GEO411/R/preprocess_coherence.R"
system(command = sprintf("Rscript %s --help", file)) # run help

cmd = sprintf("Rscript %s --directory=%s --lidar=%s --targetresolution=%s", file, directory, lidar, targetresolution)
print(cmd)
system(cmd)

# Plotting ---------------------------------------------------------------------

library(raster)
library(ggplot2)
library(tidyverse)
source("R/plot_functions.R")
library(gridExtra)
library(grid)

l = list.files(path = directory, full.names = T, recursive = T)
pre_processed_coherences = l[grepl("topophase.cor.geo.proj.resamp.na.crop.tolidar.tif$", l)]

# filter HV and HH: do not look in folders with HH in the end - yet to be implemented


# load data
ras = brick(stack(pre_processed_coherences))
chm = raster(lidar)

# Histogram --------------------------------------------------------------------
# coerce to dataframe

# Histogram creation for lidar canopy height
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

# Scatterplot ------------------------------------------------------------------
# Creating cross scatterplots for coherence and CHM for 1 date

# if needed, resample chm to cohere
chm = resample(chm, ras)

# matching extents? Otherwise run preprocessing agagin -> proprocess_coherence.R
identical(extent(chm), extent(ras))

# basic plotting
# plot(values(ras), values(chm))

# calling function from plot_functions.R
a = scatterplot(ras[[1]], chm, bins = 100,
            xlab ="Canopy Height [m]", ylab = "Coherence", title  = "2015-08-21/2015-10-02")

b = scatterplot(ras[[2]], chm, bins = 100,
            xlab ="Canopy Height [m]", ylab = "Coherence", title  = "2015-10-02/2016-02-19")

c = scatterplot(ras[[3]], chm, bins = 100,
            xlab ="Canopy Height [m]", ylab = "Coherence", title  = "2018-05-25/2018-07-20")

# HH
#
# d = scatterplot(ras[[1]], chm, bins = 100,
#                 xlab ="Canopy Height [m]", ylab = "Coherence", title  = "2015-08-21/2015-10-02")
#
# e = scatterplot(ras[[2]], chm, bins = 100,
#                 xlab ="Canopy Height [m]", ylab = "Coherence", title  = "2015-10-02/2016-02-19")
#
# f = scatterplot(ras[[3]], chm, bins = 100,
#                 xlab ="Canopy Height [m]", ylab = "Coherence", title  = "2018-05-25/2018-07-20")


g2 = grid.arrange(a, b, c, nrow = 3, top = "ALOS-2 HV Coherence",
                        bottom = grid::grid.text(
                            "Interferograms processed by ISCE",
                            gp = gpar(fontface = 3, fontsize = 9),
                            hjust = 1,
                            x = 1))
ggsave(plot = g2, filename = "CoherenceHV-CHM.svg", path = plot_dir, device = "svg", limitsize = F, width = 3, height = 9)
