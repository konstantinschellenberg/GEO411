# load libraries
library(raster)
library(osmdata)
library(ggplot2)
library(sf)

#' FNF Mask needs to be in EPSG:4326 (according to SAR-Handbook Chp. 4 Tutorial, Lei. & Siqueira)
#'
#' Forest Density Map from Copernicus Server (https://land.copernicus.eu/pan-european/high-resolution-layers/forests/tree-cover-density)
#' comes in EPSG:3035

env = getwd()

setwd("/home/robin/geodata/geo411/GEO411_FSH_Roda/ancillary_data/FNF/TCD_2015_020m_eu_03035_d05_E40N30/")
setwd("D:/Geodaten/GEO411/")

# load forest data
raw = raster("TCD_2015_020m_eu_03035_d05_E40N30.tif")
raw = raster("01_data/FNF/TCD_2015_020m_eu_03035_d05_E40N30.tif")
# load footprint
fp = st_read("/home/robin/geodata/geo411/GEO411_FSH_Roda/ancillary_data/shape_footprint/ALOS2PALSAR2_selected_GEO411.shp")
fp = st_read("02_features/footprint/ALOS2PALSAR2_selected_GEO411.shp")

# reproject shape
fp_repro = st_transform(fp, crs(raw))

# mask and crop the forest layer to footprint
masked = mask(x = raw, mask = fp_repro)
cropped = crop(masked, fp_repro)

# assign 1 to all values != 0
vals = values(cropped)
vals[vals > 0] = 1
values(cropped) = vals
fnf = cropped

# transform to EPSG:4326
fnf_repro = raster::projectRaster(fnf, crs = "+proj=longlat +datum=WGS84 +no_defs", method = "ngb")

# plot it
fnf_fac = as.factor(fnf_repro)
fnf_level = levels(fnf_fac)[[1]]
fnf_level$Forest = c("no forest", "forest")
levels(fnf_fac) = fnf_level
rasterVis::levelplot(fnf_fac, col.regions = c("white", "green"))

# write it
writeRaster(fnf_fac, "/home/robin/geodata/geo411/FNF/cropped_mask.tif", format = "GTiff")
writeRaster(fnf_fac, "01_data/FNF/cropped_mask__20m_4326.tif", format = "GTiff")

setwd(env)
