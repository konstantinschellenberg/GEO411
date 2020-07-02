#' Scatterplot of lidar canopy height and InSAR coherence and backscatter
#' init: 24.05.2020

library(raster)
library(sf)
library(ggplot2)
library(rgdal)
library(gdalUtils)
library(tidyverse)


# Input DATA -------------------------------------------------------------------

backscatter_20m_linear = "/home/robin/geodata/geo411/GEO411_FSH_Roda/snap_grd/20150821/ALOS2_20150821_LINEAR_hv_32632_20_20_cropped.tif"
backscatter_20m_db = "/home/robin/geodata/geo411/GEO411_FSH_Roda/snap_grd/20150821/ALOS2_20150821_db_hv_32632_20_20_cropped.tif"
canopy_height_model_20m = "/home/robin/geodata/geo411/GEO411_FSH_Roda/ancillary_data/chm_maskedtofnf_10m_resampled_to_20m_32632_cropped_to_intersection.tif"

# load data
bs = raster(backscatter_20m_linear)
bs[bs == 0] = NA
bs_db = raster(backscatter_20m_db)
bs_db[bs_db == 0] = NA
chm = raster(canopy_height_model_20m)
chm[chm < 0] = NA

gdalUtils::gdalinfo(backscatter)
gdalUtils::gdalinfo(canopy_height_model)

# coh = brick(list.dir[grepl("topophase.cor.geo.vrt", x = list.dir)])[[2]] # only the coherence in band 2
# lidar = raster(list.files(home_dir, full.names = T)[grepl("LAS_diff_Roda_10m_median_NA.tif", x = list.files(home_dir, full.names = T))])
# lidar = raster("D:/Geodaten/GEO411/01_data/LAS_diff_Roda_10m_median_NA.tif")

# Plotting ---------------------------------------------------------------------
plot(bs, zlim = c(0,2))
plot(chm)
plot(values(chm), values(bs_db))

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
bs_db_res = resample(bs_db, chm)

# matching extents?
identical(extent(bs_db_res), extent(chm))

df = data.frame(bs = values(bs_db_res), chm = values(chm))

# Raster Scatterplot
ggplot(df, aes(x = chm, y = bs)) +
    geom_bin2d(bins=100) +
    scale_fill_continuous(type = "viridis") +
    ggtitle("ALOS-2 HV Backscatter for 2015-08-21 vs. Lidar Canopy Height")+
    xlab("Backscatter") +
    ylab("Canopy Height [m]") +
    theme_classic()

# 2D Density map
#ggplot(df, aes(x = bs, y = chm)) +
   # stat_density_2d(aes(fill = ..density..), geom = "raster", contour = T) +
    #scale_x_continuous(expand = c(0, 0)) +
    #scale_y_continuous(expand = c(0, 0))
