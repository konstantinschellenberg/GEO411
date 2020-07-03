# preprocess backscatter
library(raster)
library(sf)
library(dplyr)

# read chm
chm = raster("/home/robin/geodata/geo411/GEO411_FSH_Roda/ancillary_data/chm_20m_maskedtofnf_32632.tif")
# locaetion on disk
backscatter_paths = list.files("/home/robin/geodata/geo411/GEO411_FSH_Roda/snap_grd/final_GRDs_SNAP", full.names = T)
backscatter_paths = backscatter_paths %>% stringr::str_subset(., pattern = "Cal_TC.tif", negate = F)
# load into list
backscatter_raster = lapply(backscatter_paths, function(x) raster(x))
# make names
names_backscatter = lapply(backscatter_raster, function(x) paste0("bs_", substr(names(x), start=40, stop=45)))
# apply names
names(backscatter_raster) = names_backscatter


# reproject data to 32632 and set no data to 0
for (i in seq_along(backscatter_paths)){
    path = "/home/robin/geodata/geo411/GEO411_FSH_Roda/snap_grd/32632_NoData0_GRDS"
    if(!dir.exists(path)) dir.create(path)
    base_name = strsplit(backscatter_paths[[i]], .Platform$file.sep)[[1]] %>% tail(., n=1) %>% substr(., 40,45)
    ending = ".tif"
    file_name = paste(path, paste0(base_name, "_32632", ending), sep = .Platform$file.sep)
    cmd = sprintf("gdalwarp -t_srs EPSG:32632 -srcnodata 0 -dstnodata 0 %s %s", backscatter_paths[[i]], file_name)
    system(cmd)
}



# load data
# naming convention
backscatter_32632_paths = list.files("/home/robin/geodata/geo411/GEO411_FSH_Roda/snap_grd/32632_NoData0_GRDS", full.names = T)
backscatter_32632_raster = lapply(backscatter_32632_paths, function(x) raster(x))
names_backscatter = lapply(backscatter_32632_paths, function(x) paste0("bs_", substr(x, start=72, stop=77)))
names(backscatter_32632_raster) = names_backscatter


# Resample all to the first backscatter
for(i in seq_along(backscatter_32632_raster)){
    if(i!=1){
        backscatter_32632_raster[[i]] = resample(backscatter_32632_raster[[i]], backscatter_32632_raster[[1]])
    }
}

###
# plot data
###
# plot extents
# raster::extent doesn't work here..
backscatter_extents = vector("list", length(backscatter_32632_raster))
for(i in seq_along(backscatter_32632_raster)){
    bs_sf = st_bbox(backscatter_32632_raster[[i]]) %>% st_as_sfc(., crs=crs(backscatter_32632_raster[[i]]))
    backscatter_extents[[i]] = bs_sf
}

for(i in seq_along(backscatter_extents)){
    print(i)
    if(i == 1) plot(backscatter_extents[[i]], axes=T, expandBB=c(1,1,1,1))
    else plot(backscatter_extents[[i]], add = T)
}


############

# write resampled data

resampled_backscatter_path = "/home/robin/geodata/geo411/GEO411_FSH_Roda/snap_grd/resampeldToFirst_32632"
if(!dir.exists(resampled_backscatter_path)) dir.create(resampled_backscatter_path)
for (i in seq_along(backscatter_32632_raster)){
    name = names(backscatter_32632_raster)[[i]]
    path = paste(resampled_backscatter_path, paste0(name, "_resampeldToFirst.tif"), sep=.Platform$file.sep)
    writeRaster(backscatter_32632_raster[[i]], path)
}



# get all backscatter
backscatter_paths = list.files("/home/robin/geodata/geo411/GEO411_FSH_Roda/snap_grd/32632_NoData0_GRDS/", full.names = T)
resolutions = c("50 50", "100 100")
for (i in resolutions){
    for (j in seq_along(backscatter_paths)){
        name_new = paste0(strsplit(backscatter_paths[[j]], .Platform$file.sep)[[1]] %>%  tail(., n=1) %>% substr(., start = 1, stop = nchar(.)-4), "_32632", "_", substr(i,1,2), "m", ".tif")
        dir_new = "/home/robin/geodata/geo411/GEO411_FSH_Roda/snap_grd/backscatter_resampled_50_100m"
        if(!dir.exists(dir_new)) dir.create(dir_new)
        name_new = paste(dir_new, name_new, sep = .Platform$file.sep)
        cmd = sprintf("gdalwarp -r med -tr %s  %s %s", i, backscatter_paths[[j]], name_new)
        print(cmd)
        system(cmd)

    }
}
