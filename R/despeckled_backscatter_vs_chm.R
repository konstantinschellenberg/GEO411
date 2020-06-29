# load librarys
library(dplyr)
library(raster)
library(ggplot2)
library(mapview)
library(sf)


# set working directory
# next time data in the project and on .gitignore
setwd("/home/robin/geodata/geo411/GEO411_FSH_Roda/")

# data paths
bs_path = "20150821_20151002/20150821/ALOS2-FBDR1_1__A-ORBIT__ALOS2067251007-150821_Cal_Spk_TC.tif"
chm_path = "ancillary_data/chm_maskedtofnf_10m_resampled_to_20m_32632_-99nodata.tif"

# Reproject Backscatter
reprojcted_backscatter_path = "20150821_20151002/20150821/bs_despeckled_32632_20x20.tif"
if(!file.exists(reprojcted_backscatter_path)){
    cmd = sprintf("gdalwarp -t_srs EPSG:32632 -tr 20 20 %s %s", bs_path, reprojcted_backscatter_path)
    system(cmd)
}

# load data
chm = raster(chm_path)
bs_HV = raster(reprojcted_backscatter_path)

# plot extents
# raster::extent doesn't work here...
bs_sf = st_bbox(bs_HV) %>% st_as_sfc(., crs=crs(bs_HV))
chm_sf = st_bbox(chm) %>% st_as_sfc(., crs=crs(bs_HV))
# plot the extents
plot(bs_sf, col="red", axes = T, main = "Extents of Backscatter and Canopy Height Model (Lidar)")
plot(chm_sf, col="blue", add = T)
# Calculate Intersection
intersec = st_intersection(bs_sf, chm_sf)
intersec = st_sf(intersec)
plot(intersec, add =T, col="green")
legend(780000,5600000, legend = c("Backscatter", "CHM", "Intersection"), col=c("red", "blue", "green"), pch=20)


# CROP AND MASK BOTH --> Doesn't work...
# crop_bs = crop(bs_HV, intersec)
# crops_bs = mask(crop_bs, intersec)
# crop_chm = crop(chm, intersec)
# crop_chm = mask(crop_chm, intersec)


# set all values to NA in Backscatter as SNAP doesn't give any specifc NA value
bs_HV[bs_HV == 0] = NA
# set all value in Backscatter to NA where CHM = NA
bs_HV[chm == NA] = NA
# set all values in CHM to NA where Backscatter is NA
chm[bs_HV == NA] = NA


# Resample One to each other
res_bs_HV = resample(bs_HV, chm)

# Make Histogramm of Backscater
h = hist(res_bs_HV[res_bs_HV < 1], breaks=50, maxpixels=1000000)
h$counts = h$counts * 400/10000
h_data = data.frame(breaks=h$mids, counts=h$counts)

# hist plotting
ggplot(h_data, aes(x = breaks, y = counts)) +
    geom_bar(stat = "identity", fill='darkgreen', alpha = 0.8) +
    ggtitle("Backscatter Height Distribution") +
    xlab("Backscatter [Sigma0]")+
    ylab("Count") +
    theme_classic()

##################################
# Scatterplot
##################################
# matching extents
identical(extent(res_bs_HV), extent(chm))
# make it a dataframe
df = data.frame(bs = values(res_bs_HV), chm = values(chm))
# make ggploooooot
# Raster Scatterplot
ggplot(df, aes(x = chm, y = bs)) +
    geom_bin2d() +
    scale_fill_continuous(type = "viridis") +
    ggtitle("ALOS-2 HV Backscatter for 2015-08-21 vs. Lidar Canopy Height")+
    xlab("Backscatter") +
    ylab("Canopy Height [m]") +
    theme_classic()


plot(values(chm), values(res_bs_HV), ylim=c(0,1))
