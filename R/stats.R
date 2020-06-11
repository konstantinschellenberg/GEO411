#' Scatterplot of lidar canopy height and InSAR coherence and backscatter
#' init: 24.05.2020

library(raster)
library(sf)
library(ggplot2)
library(rgdal)
library(gdalUtils)
library(tidyverse)

# ------------------------------------------------------------------------------

home_dir = "D:/Geodaten/GEO411/01_data"
dir = paste(home_dir, "20151002", sep = "/")

list.dir = list.files(dir, full.names = T)

# Input DATA -------------------------------------------------------------------

coherence = "D:/Geodaten/GEO411/01_data/20150821_20151002/interferogram/topophase_cor_geo_32632_resamp30_na0.tif"
canopy_height_model = "D:/Geodaten/GEO411/01_data/chm_maskedtofnf_30_32632_-99nodata.tif"

# load data
ras = raster(coherence)
chm = raster(canopy_height_model)

gdalUtils::gdalinfo(coherence)
gdalUtils::gdalinfo(canopy_height_model)

# coh = brick(list.dir[grepl("topophase.cor.geo.vrt", x = list.dir)])[[2]] # only the coherence in band 2
# lidar = raster(list.files(home_dir, full.names = T)[grepl("LAS_diff_Roda_10m_median_NA.tif", x = list.files(home_dir, full.names = T))])
# lidar = raster("D:/Geodaten/GEO411/01_data/LAS_diff_Roda_10m_median_NA.tif")

# Plotting ---------------------------------------------------------------------
# basic

# plot(coh)
# plot(lidar)
# plot(chm)

# Sophisticated plotting -------------------------------------------------------

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

# resample coherence to canopy height
ras.res = resample(ras, chm)

# matching extents?
identical(extent(chm), extent(ras.res))

plot(values(ras.res), values(chm))

df = data.frame(ras = values(ras.res), chm = values(chm))

# Raster Scatterplot
ggplot(df, aes(x = ras, y = chm)) +
    geom_bin2d(bins = 100) +
    scale_fill_continuous(type = "viridis") +
    ggtitle("ALOS-2 HV Coherence for 2015-08-21/2015-10-02 over Roda")+
    xlab("Coherence") +
    ylab("Canopy Height [m]") +
    theme_classic()

# 2D Density map
ggplot(df, aes(x = ras, y = chm)) +
    stat_density_2d(aes(fill = ..density..), geom = "raster", contour = T) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0))
