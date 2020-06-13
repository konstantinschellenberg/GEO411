#' Scatterplot of lidar canopy height and InSAR coherence and
#' init: 24.05.2020

# Preprocess -------------------------------------------------------------------

require("optparse", attach.required = T)

# if needed, run the coherence preprocessing steps in preprocess_coherence.R
file = "D:/Geodaten/Master/projects/GEO411/R/preprocess_coherence.R"
system(command = sprintf("Rscript %s --help", file)) # run help

# user inputs
directory = "D:/Geodaten/GEO411/01_data"
lidar = "D:/Geodaten/GEO411/01_data/chm_maskedtofnf_30_32632_-99nodata.tif"
targetresolution = "20"

cmd = sprintf("Rscript %s --directory=%s --lidar=%s --targetresolution=%s", file, directory, lidar, targetresolution)
print(cmd)
system(cmd)

# Plotting ---------------------------------------------------------------------

library(raster)
library(ggplot2)
library(tidyverse)
source("R/plot_functions.R")

directory = "D:/Geodaten/GEO411/01_data"
l = list.files(path = directory, full.names = T, recursive = T)
pre_processed_coherences = l[grepl("topophase.cor.geo.proj.resamp.na.crop.tolidar.tif$", l)]

canopy_height_model = "D:/Geodaten/GEO411/01_data/chm_maskedtofnf_30_32632_-99nodata.tif"

# load data
ras = raster(pre_processed_coherences[1])
chm = raster(canopy_height_model)

# check with gdalinfo, mind the same extent and resolution!
gdalUtils::gdalinfo(pre_processed_coherences[1])
gdalUtils::gdalinfo(canopy_height_model)

# if needed, resample chm to cohere
chm = resample(chm, ras)

# Histogram --------------------------------------------------------------------
# coerce to dataframe

# Histogram creation for lidar canopy height
h = hist(chm, breaks = 50, maxpixels = 500000)
h$counts = h$counts * 400 / 10000
h_data = data.frame(breaks = h$mids, counts = h$counts)

# hist plotting
ggplot(h_data, aes(x = breaks, y = counts)) +
    geom_bar(stat = "identity", fill='darkgreen', alpha = 0.8, width = .5) +
    ggtitle("Lidar Canopy Height Distribution in Roda Catchment") +
    xlab("Canopy Height [m]")+
    ylab("Area [ha]") +
    theme_classic()

# Scatterplot ------------------------------------------------------------------
# Creating cross scatterplots for coherence and CHM for 1 date

# matching extents? Otherwise run preprocessing agagin -> proprocess_coherence.R
identical(extent(chm), extent(ras))

# basic plotting
plot(values(ras), values(chm))

# calling function from plot_functions.R
scatterplot(ras, chm, bins = 100)
