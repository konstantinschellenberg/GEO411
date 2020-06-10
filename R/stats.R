#' Scatterplot of lidar canopy height and InSAR coherence and backscatter
#' init: 24.05.2020

library(raster)
library(sf)
library(ggplot2)
library(rgdal)
library(gdalUtils)

p.r = "D:/Geodaten/GEO411/03_development/stats/date1_clip.vrt"
p.r.res = "D:/Geodaten/GEO411/03_development/stats/date1_clip_res.vrt"
p.l = "D:/Geodaten/GEO411/01_data/LAS_diff_Roda_10m_median_NA.tif"
p.l.res = "D:/Geodaten/GEO411/01_data/LAS_diff_Roda_10m_median_NA_res.vrt"

# r = brick("D:/Geodaten/GEO411/03_development/stats/date1_clip.vrt")[[2]]
# l = raster("D:/Geodaten/GEO411/01_data/LAS_diff_Roda_10m_median_NA.tif")

ex = st_read("D:/Geodaten/GEO411/02_features/roi.gpkg", layer = "lidar_extent")

r = brick(p.r)[[2]]
r.res = brick(p.r.res) %>% crop(ex)
l = raster(p.l)
l.res = raster(p.l.res) %>% crop(ex)

r.v = values(r.res) %>% .[1:1000]
l.v = values(l.res) %>% .[1:1000]

plot(r.v)
plot(r.v, l.v)

ggplot() +
    geom_point(aes(x = values(r[[2]]), y = values(lidar)))

