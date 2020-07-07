##################################
# Script for preprocessing all data
##################################

library(raster)
library(sf)
library(dplyr)
library(data.table)
library(ggplot2)


####################################
##  Mask out non-Forest from CHM  ##
####################################

fnf_path = "/home/robin/geodata/geo411/GEO411_FSH_Roda/ancillary_data/FNF/TCD_2015_020m_eu_03035_d05_E40N30/TCD_2015_020m_eu_03035_d05_E40N30.tif"
chm_path = "/home/robin/geodata/geo411/GEO411_FSH_Roda/ancillary_data/LAS_diff_Roda_10m_median_NA.tif"
bs_dir = "/home/robin/geodata/geo411/GEO411_FSH_Roda/snap_grd/final_GRDs_SNAP/"

# read both
fnf = raster(fnf_path)
chm = raster(chm_path)
# check for crs
crs(fnf)
crs(chm)
# compare CRSs --> evaluates to FALSE
raster::compareCRS(fnf, chm)

# reproject CHM (32632) to FNF (3035) as the other way round takes too long
# Needs to be reprojected tp 32632 later
chm = raster::projectRaster(chm, crs = crs(fnf))

# Now TRUE
raster::compareCRS(chm, fnf)

# CROP and mask Fnf to CHM
# compute shape for chm
chm_shp = st_bbox(chm) %>% st_as_sfc(., crs=crs(chm)) %>% st_sf(.)
# crop fnf
fnf_crop = crop(fnf, chm_shp)
fnf_masked = mask(fnf_crop, chm_shp)

# resample chm (10x10) to match resolution of fnf (20x20) and extent
chm = resample(chm, fnf_masked)

# Set all Pixels that are NA in the fnf to NA in the CHM (urban structures that are still present in the chm)
chm_fnf = chm
chm_fnf[fnf_masked == 0] = NA
# check the numer of pixels with NA before and after masking with fnf
sum(is.na(values(chm)))
sum(is.na(values(chm_fnf)))

# set all values < 0 in chm to NA
chm_fnf[chm_fnf < 0] = NA
sum(is.na(values(chm_fnf)))


# reproject it back to 32632 and write it out
chm_fnf_repro = raster::projectRaster(chm_fnf, crs = crs(raster(chm_path)))
writeRaster(chm_fnf_repro, "/home/robin/geodata/geo411/GEO411_FSH_Roda/ancillary_data/chm_20m_maskedtofnf_32632.tif", overwrite=T)


# Read the CHM back in and resample it to 50 and 100
chm_new_path  = "/home/robin/geodata/geo411/GEO411_FSH_Roda/ancillary_data/chm_20m_maskedtofnf_32632.tif"
chm_50m_path = "/home/robin/geodata/geo411/GEO411_FSH_Roda/ancillary_data/chm_50m_maskedtofnf_32632.tif"
chm_100m_path = "/home/robin/geodata/geo411/GEO411_FSH_Roda/ancillary_data/chm_100m_maskedtofnf_32632.tif"
chm_200m_path = "/home/robin/geodata/geo411/GEO411_FSH_Roda/ancillary_data/chm_200m_maskedtofnf_32632.tif"
cmd = sprintf("gdalwarp -tr 50 50 %s %s", chm_new_path, chm_50m_path)
cmd2 = sprintf("gdalwarp -tr 100 100 %s %s", chm_new_path, chm_100m_path)
cmd3 = sprintf("gdalwarp -tr 200 200 %s %s", chm_new_path, chm_200m_path)
system(cmd)
system(cmd2)
system(cmd3)

# move both CHMs to git repo
# find git datadirectory
a = list.dirs(path.expand("~"))
datadir = grep("GEO411/data/ancillary", a, value = T)
cmd50 = sprintf("cp %s %s", chm_50m_path, datadir)
cmd100 = sprintf("cp %s %s", chm_100m_path, datadir)
cmd200 = sprintf("cp %s %s", chm_200m_path, datadir)
system(cmd200)
system(cmd100)
system(cmd50)



##############################
##  Preprocess Backscatter  ##
##############################


# read 20m chm
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


# reproject backscatter to 32632 and set no data to 0
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
# name it
backscatter_32632_paths = list.files("/home/robin/geodata/geo411/GEO411_FSH_Roda/snap_grd/32632_NoData0_GRDS", full.names = T)
backscatter_32632_raster = lapply(backscatter_32632_paths, function(x) raster(x))
names_backscatter = lapply(backscatter_32632_paths, function(x) paste0("bs_", substr(x, start=72, stop=77)))
names(backscatter_32632_raster) = names_backscatter


##*************************
##  NON-Matching-Extent  **
##*************************

# will be resolved later

# # Resample all to the first backscatter
# for(i in seq_along(backscatter_32632_raster)){
#     if(i!=1){
#         backscatter_32632_raster[[i]] = resample(backscatter_32632_raster[[i]], backscatter_32632_raster[[1]])
#     }
# }


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
#
# # write resampled data
# resampled_backscatter_path = "/home/robin/geodata/geo411/GEO411_FSH_Roda/snap_grd/resampeldToFirst_32632"
# if(!dir.exists(resampled_backscatter_path)) dir.create(resampled_backscatter_path)
# for (i in seq_along(backscatter_32632_raster)){
#     name = names(backscatter_32632_raster)[[i]]
#     path = paste(resampled_backscatter_path, paste0(name, "_resampeldToFirst.tif"), sep=.Platform$file.sep)
#     writeRaster(backscatter_32632_raster[[i]], path)
# }

##################################################################################


# get all backscatter
# and reprohect them to 50 and 100m
backscatter_paths = list.files("/home/robin/geodata/geo411/GEO411_FSH_Roda/snap_grd/32632_NoData0_GRDS/", full.names = T)
resolutions = c("50 50", "100 100", "200 200")
for (i in resolutions){
    for (j in seq_along(backscatter_paths)){
        name_new = paste0(strsplit(backscatter_paths[[j]], .Platform$file.sep)[[1]] %>%  tail(., n=1) %>% substr(., start = 1, stop = nchar(.)-4), "_32632", "_", substr(i,1,2), "m", ".tif")
        dir_new = "/home/robin/geodata/geo411/GEO411_FSH_Roda/snap_grd/backscatter_resampled_50_100_200m"
        if(!dir.exists(dir_new)) dir.create(dir_new)
        name_new = paste(dir_new, name_new, sep = .Platform$file.sep)
        cmd = sprintf("gdalwarp -r med -tr %s  %s %s", i, backscatter_paths[[j]], name_new)
        print(cmd)
        system(cmd)

    }
}


########
# move data to git repo
########
# find BSC directory
datadir = grep("/data/BSC", a, value = T)
# copy the backscatter to there
cmd = sprintf("cp /home/robin/geodata/geo411/GEO411_FSH_Roda/snap_grd/backscatter_resampled_50_100_200m/* %s", datadir)
system(cmd)



############################
##  Preprocess Coherence  ##
############################

files = list.files("/home/robin/geodata/geo411/GEO411_FSH_Roda", pattern = "topophase.cor.geo$", full.names = T, recursive = T)
files = grep("HH|test", files, invert = T, value = T)

# gdal_transalte coherence to make tif
new_dir = "/home/robin/geodata/geo411/GEO411_FSH_Roda/coherences"
if(!dir.exists(new_dir)) dir.create(new_dir)
dates = sapply(files, function(x) substr(x, 44, 60))
files_new = paste(new_dir, paste0(dates, ".tif"), sep = .Platform$file.sep)
# translate them
for(i in seq_along(files)){
    cmd = sprintf("gdal_translate -b 2 -a_nodata 0 %s %s", files[[i]], files_new[[i]])
    system(cmd)
}

# gdawarp them to 50 and 100m

resolutions = c("50 50", "100 100", "200 200")
files = list.files("/home/robin/geodata/geo411/GEO411_FSH_Roda/coherences", full.names = T)
for (i in seq_along(resolutions)){
    for (j in seq_along(files)){
        base_path =  strsplit(files[[j]], split = .Platform$file.sep)
        base_path = paste(base_path[[1]][1:lengths(base_path)-1], collapse = .Platform$file.sep)
        date = substr(files[[j]], 55, 71)
        file_path = paste(base_path, paste0(date, "_32632_", substr(resolutions[[i]],1,2), ".tif"), sep = .Platform$file.sep)
        cmd = sprintf("gdalwarp -tr %s -r med -t_srs EPSG:32632 %s %s", resolutions[[i]], files[[j]], file_path)
        system(cmd)
    }}

# move coherences to git directory
# find git datadirectory
datadir = grep("/data/COH", a, value = T)
cmd = sprintf("cp /home/robin/geodata/geo411/GEO411_FSH_Roda/coherences/* %s", datadir)
system(cmd)


#######################
##  MAKE DATAFRAMES  ##
#######################

# LOAD DATA --------------------------------------------------------------------
# Load 50m rasters
chm50 = raster("data/ancillary/chm_50m_maskedtofnf_32632.tif")
chm100 = raster("data/ancillary/chm_100m_maskedtofnf_32632.tif")
chm200 = raster("data/ancillary/chm_200m_maskedtofnf_32632.tif")

bsc50_path = list.files("data/BSC", recursive = TRUE, pattern = "32632_50m", full.names = TRUE)
bsc100_path = list.files("data/BSC", recursive = TRUE, pattern = "32632_10m", full.names = TRUE)
bsc200_path = list.files("data/BSC", recursive = TRUE, pattern = "32632_20m", full.names = TRUE)
coh50_path = list.files("data/COH", recursive = TRUE, pattern = "32632_50", full.names = TRUE)
coh100_path = list.files("data/COH", recursive = TRUE, pattern = "32632_10", full.names = TRUE)
coh200_path = list.files("data/COH", recursive = TRUE, pattern = "32632_10", full.names = TRUE)

list_path50 = c(bsc50_path, coh50_path)
list_path100 = c(bsc100_path, coh100_path)
list_path200 = c(bsc100_path, coh100_path)

list_raster50 = vector(mode = "list", length = length(unlist(list_path50)))
list_raster100 = vector(mode = "list", length = length(unlist(list_path100)))
list_raster200 = vector(mode = "list", length = length(unlist(list_path200)))
list_extent = vector(mode = "list", length = length(unlist(list_path50)))

names = c("chm",
          "bs_150821",
          "bs_151002",
          "bs_160219",
          "bs_180525",
          "bs_180720",
          "coh_15_15",
          "coh_15_16",
          "coh_18_18",
          "x",
          "y")

# Preprocess data to extent and list them --------------------------------------

sf = NULL
for (i in seq_along(list_path50)){
    print(list_path50[i])
    new_sf = raster(list_path50[i]) %>% st_bbox() %>% st_as_sfc() %>% st_as_sf()
    sf = rbind(sf, new_sf)
}

inter = st_intersection(sf) # %>% mutate(area = st_area(.) / 10000)
extent = inter[inter$n.overlaps == 8, ]
extent = raster::extent(extent)

dummy_raster50 = raster(ext = extent, resolution = 50, crs = crs(chm50))
dummy_raster100 = raster(ext = extent, resolution = 100, crs = crs(chm50))
dummy_raster200 = raster(ext = extent, resolution = 200, crs = crs(chm50))

# RESAMPLE to extent intersecting with all data
# not necessary
rasters50 = lapply(unlist(list_path50), function(x){
    raster(x) %>% resample(dummy_raster50)
})

rasters100 = lapply(unlist(list_path100), function(x){
    raster(x) %>% resample(dummy_raster100)
})

rasters200 = lapply(unlist(list_path200), function(x){
    raster(x) %>% resample(dummy_raster200)
})


chm50 = chm50 %>% resample(dummy_raster50)
chm100 = chm100 %>% resample(dummy_raster100)
chm200 = chm200 %>% resample(dummy_raster200)

list_chm_backscatter_coherence50 = do.call(c, list(chm50, rasters50))
list_chm_backscatter_coherence100 = do.call(c, list(chm100, rasters100))
list_chm_backscatter_coherence200 = do.call(c, list(chm200, rasters200))

# Make DataFrame ---------------------------------------------------------------

list_all = list(list_chm_backscatter_coherence50, list_chm_backscatter_coherence100, list_chm_backscatter_coherence200)
dfs = vector("list", length = length(list_all))
final_df = NULL

for (h in seq_along(list_all)){
    list = list_all[[h]]
    print(h)


    # make final df
    for (i in seq_along(list)){
        print(names(list[[i]]))
        # if i == 1 make dataframe
        if(i == 1){
            final_df = as.data.frame(list[[i]])
        }else if (i < length(list)){
            final_df[[i]] = values(list[[i]])
        }else{
            final_df[[i]] = values(list[[i]])
            xy = as.data.frame(list[[i]], xy = T) %>% dplyr::select(x, y)
            final_df = cbind(final_df, xy)
        }
    }
    names(final_df) = names
    dfs[[h]] = final_df
}

# rename the master lists
names(dfs) = c("df50", "df100", "df200")

# save full dataset with all NAs and CHM
if (!file.exists("model/dataframes/final_df.RDS")){
    saveRDS(dfs, "model/dataframes/final_df.RDS")
}

# assign a new master dataset variable for later use
master = dfs

##########################
# 2. PREDICTION DATASET
##########################

for (i in seq_along(master)){
    df = master[[i]]
    dfs[[i]] = select(df, -chm)
    cat("Canopy Height removed from dataset")
}

# save prediction dataset without CHM
if (!file.exists("model/dataframes/predictionset.RDS")){
    saveRDS(dfs, "model/dataframes/predictionset.RDS")
}

##########################
# 3. MODELLING DATASET
##########################

# IMPUTE
# throw out all lines where there is one NA (not sure if this is the best way to impute data;)

for (i in seq_along(master)){
    df = master[[i]]
    df = df[complete.cases(df),]

    # still 0s inside, gdalwarp: dstnodata = 0 !!
    dfs[[i]] = df
    cat("NAs removed from the dataset\n")
}

# dataset with removed NAs
if (!file.exists("model/dataframes/trainingtestset.RDS")){
    saveRDS(dfs, "model/dataframes/trainingtestset.RDS")
}
# (End)
