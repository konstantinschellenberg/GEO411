#######################
# Create linear model to predict forest heigth given the radar backscatter
#######################

# load libraries
library(dplyr)
library(raster)
library(tools)
library(stringr)

# set working directory
setwd("/home/robin/geodata/geo411/GEO411_FSH_Roda/")
setwd("D:/Geodaten/GEO411/01_data/")

# if this runs, gdal is properly installed on the machine. No conda environement possible. Needs
# to be installed as binary
system("gdalinfo --version")

###############################################################################
###############################################################################

###########################
##  BACKSCATTER, LINEAR  ##
###########################

# get all the backscatter-files
pattern = "Cal_TC.tif$"
files = list.files(".", pattern = pattern, recursive = T)

# reproject all data to 32632 and 20 x 20 m
for (i in 1:length(files)){
    file_name = strsplit(files[[i]], .Platform$file.sep)[[1]] %>% tail(., n=1) %>% substr(., start = 1, stop = nchar(.)-4)
    file_name = paste0(file_name, "_32632_20x20")
    file_ending = ".tif"
    path = unlist(strsplit(files[[i]], .Platform$file.sep))[1:2]
    base_path = paste(path[[1]], sep = .Platform$file.sep)
    file_path = paste(base_path, paste0(file_name, file_ending), sep = .Platform$file.sep)
    if(!file.exists(file_path)){
        cmd = sprintf("gdalwarp -t_srs EPSG:32632 -tr 20 20 %s %s", files[[i]], file_path)
        system(cmd)
        }else{
        print("Reprojected, 20 x 20 m, Backscatter already exists")
    }
}


# resample all data to make extent exactly the same metadata
# reference files
ref_file_path = "GRDS/ALOS2-FBDR1_1__A-ORBIT__ALOS2067251007-150821_Cal_TC_32632_20x20.tif"
ref_file = raster(ref_file_path)
# get all files
pattern = "Cal_TC_32632_20x20.tif$"
resampled_files = list.files(".", pattern=pattern, recursive = T)
# load and resample all files
backscatter_list = vector("list", length(resampled_files))
backscatter_list[[1]] = ref_file
for (i in 1:length(resampled_files)){
    if(!resampled_files[[i]] == ref_file_path){
        ras = raster(resampled_files[[i]])
        ras = resample(ras, ref_file)
        backscatter_list[[i]] = ras
    }
}

# stack em
backscatter_stack = raster::stack(backscatter_list)


#################
##  COHERENCE  ##
#################

# get all the coherence files
pattern = "topophase.cor.geo.vrt$"
coherence_files = list.files(".", pattern = pattern, recursive = TRUE)
coherence_files = coherence_files %>% .[str_detect(., "HH", negate=TRUE)] %>% .[str_detect(., "^201")]

# make a list where to store them
coherence_raster_list = vector("list", length(coherence_files))

# first make a tiff of all of them
# I think isce2geotiff would do soemthing like this as well
for (i in 1:length(coherence_files)){
    file_name = strsplit(coherence_files[[i]], .Platform$file.sep)[[1]] %>% tail(., n=1) %>% substr(., start = 1, stop = nchar(.)-4)
    file_ending = ".tif"
    path = unlist(strsplit(coherence_files[[i]], .Platform$file.sep))[1:2]
    base_path = paste(path[[1]], path[[2]], sep = .Platform$file.sep)
    file_path = paste(base_path, paste0(file_name, file_ending), sep = .Platform$file.sep)
    # build gdal translate
    if(!file.exists(file_path)){
    cmd = sprintf("gdal_translate -b 2 %s %s", coherence_files[[i]], file_path)
    system(cmd)}else print("GeoTiff of Coherence already exists")
}

# get all the just produced coherence tifs
pattern = "topophase.cor.geo.tif$"
coherence_files_tif = list.files(".", pattern = pattern, recursive = TRUE)

# now reproject and resample
for (i in 1:length(coherence_files_tif)){
    file_name = strsplit(coherence_files_tif[[i]], .Platform$file.sep)[[1]] %>% tail(., n=1) %>% substr(., start = 1, stop = nchar(.)-4)
    file_name = paste0(file_name, "_32632_20x20")
    file_ending = ".tif"
    path = unlist(strsplit(coherence_files_tif[[i]], .Platform$file.sep))[1:2]
    base_path = paste(path[[1]], path[[2]], sep = .Platform$file.sep)
    file_path = paste(base_path, paste0(file_name, file_ending), sep = .Platform$file.sep)
    if(!file.exists(file_path)){
    # build gdalwarp command
    cmd = sprintf("gdalwarp -t_srs EPSG:32632 -tr 20 20 %s %s", coherence_files_tif[[i]], file_path)
    system(cmd)}else print("20m Coherence in EPSG:32632 aleady exists")
}

# resample all coherence files to the ref backscatter to exactly match extents
pattern = "topophase.cor.geo_32632_20x20.tif"
coherence_files_resampled_tif = list.files(".", pattern = pattern, recursive = T)
for (i in 1:length(coherence_files_resampled_tif)){
    coh_ras = raster(coherence_files_resampled_tif[[i]])
    coh_ras = resample(coh_ras, ref_file)
    coherence_raster_list[[i]] = coh_ras
    names(coherence_raster_list)[[i]] = coherence_files_resampled_tif[[i]]
}

# stack em
coherence_stack = raster::stack(coherence_raster_list)

################################################################################
################################################################################

# load chm
chm = raster("ancillary_data/chm_maskedtofnf_10m_resampled_to_20m_32632.tif")
chm = raster("ancillary_data/chm_maskedtofnf_10m_resampled_to_20m_32632_-99nodata.tif") # already NA = -99


# resample all backscatter to the chm
for (i in 1:length(backscatter_list)){
    backscatter = resample(backscatter_list[[i]], chm)
    backscatter_list[[i]] = backscatter
    if(extent(backscatter_list[[i]]) == extent(chm)){
        print(sprintf("CHM and Backscatter: %s are equal", backscatter))
    }
}

# resample all coherences to chm
for (i in 1:length(coherence_raster_list)){
    coherence = resample(coherence_raster_list[[i]], chm)
    coherence_raster_list[[i]] = coherence
    if (extent(coherence_raster_list[[i]]) == extent(chm)){
        print(sprintf("CHM and Coherence: %s are equal", coherence))
    }
}

#####
# Make DataFrame
#####
# list all
list_chm_backscatter_coherence = do.call(c, list(chm, backscatter_list, coherence_raster_list))
# get the names
names_backscatter = sapply(backscatter_list, function(x) names(x))
names_coherence = sapply(coherence_raster_list, function(x) names(x))
names_all = c("chm", names_backscatter, names_coherence)
# assign the names
names(list_chm_backscatter_coherence) = names_all

# make final df
for (i in 1:length(list_chm_backscatter_coherence)){
    # if i == 1 make dataframe
    if(i == 1){
        final_df = as.data.frame(list_chm_backscatter_coherence[[i]])
    }else if (i < length(list_chm_backscatter_coherence)){
    final_df[[i]] = as.data.frame(list_chm_backscatter_coherence[[i]])
    }else{
        final_df[[i]] = as.data.frame(list_chm_backscatter_coherence[[i]], xy = TRUE) %>%
            dplyr::select(last_col(), x, y)
    }
}

# name the columns
colnames(final_df)[1:length(list_chm_backscatter_coherence)] = names_all
