#' Scatterplot of lidar canopy height and InSAR coherence and backscatter
#' init: 24.05.2020

library(raster)
library(sf)
library(ggplot2)
library(rgdal)
library(gdalUtils)
library(tidyverse)
library(mapview)


# Input DATA -------------------------------------------------------------------

backscatter_20m_linear = "/home/robin/geodata/geo411/GEO411_FSH_Roda/snap_grd/20150821/ALOS2-FBDR1_1__A-ORBIT__ALOS2067251007-150821_Cal_TC_23632_20_20m.tif"
canopy_height_model_20m = "/home/robin/geodata/geo411/GEO411_FSH_Roda/ancillary_data/chm_maskedtofnf_10m_resampled_to_20m_32632.tif"

# load data
bs = raster(backscatter_20m_linear)
bs[bs == 0] = NA
chm = raster(canopy_height_model_20m)
chm[chm < 0] = NA

#gdalUtils::gdalinfo(backscatter_20m_linear)
#gdalUtils::gdalinfo(canopy_height_model_20m)

# the backscatter was reprojected and resamples on the command line using gdalwarp
# gdalwarp -t_srs EPSG:32632 -tr 20 20 ALOS2-FBDR1_1__A-ORBIT__ALOS2067251007-150821_Cal_TC.tif ALOS2-FBDR1_1__A-ORBIT__ALOS2067251007-150821_Cal_TC_23632_20_20m.tif
compareCRS(bs, chm)

# crop backscatter to chm
bs = resample(bs, chm)

# plot values
#plot(values(chm), values(bs), ylim=c(0,2))

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

# matching extents?
identical(extent(bs), extent(chm))

df = data.frame(bs = values(bs), chm = values(chm))

# Raster Scatterplot
ggplot(df, aes(x = chm, y = bs)) +
    geom_bin2d(bins=70) +
    ylim(0,2) +
    scale_fill_continuous(type = "viridis") +
    ggtitle("ALOS-2 HV Backscatter for 2015-08-21 vs. Lidar Canopy Height")+
    xlab("Canopy Height") +
    ylab("Backscatter") +
    theme_classic()

