############
# Regress Lidar Canopy Height onto Alos-2 Backscatter and Coherence
############

# load libraries
library(raster)
library(dplyr)
library(mapview)

##########
# load data
##########

# backscatter
backscatter_paths = list.files("data/BSC", full.names = TRUE)

# 50m path
backscatter_50_path = grep("50m", backscatter_paths, value = TRUE)
# 50m names
backscatter_50_names = lapply(backscatter_50_path, function(x) substr(x, 10,15))
# 50m raster
backscatter_50_raster = lapply(backscatter_50_path, raster)


# 100m path
backscatter_100_path = grep("10m", backscatter_paths, value = TRUE)
# 100m names
backscatter_100_names = lapply(backscatter_100_path, function(x) substr(x, 10,15))
# raster
backscatter_100_raster = lapply(backscatter_100_path, raster)

# Coherences
coherence_path = list.files("data/COH", full.names = TRUE)
# 50m path
coherence_50_path = grep("_50.tif", coherence_path, value = TRUE)
# 50m names
coherence_50_names = lapply(coherence_50_path, function(x) substr(x, 10,26))
# 50m raster
coherence_50_raster = lapply(coherence_50_path, raster)

# 100m path
coherence_100_path = grep("_10.tif", coherence_path, value = TRUE)
# 100m names
coherence_100_names = lapply(coherence_100_path, function(x) substr(x, 10,26))
# 100m raster
coherence_100_raster = lapply(coherence_100m_path, raster)


# CHMs
chm_50_path = "data/ancillary/chm_50m_maskedtofnf_32632.tif"
chm_50 = raster(chm_50m_path)
chm_100_path = "data/ancillary/chm_100m_maskedtofnf_32632.tif"
chm_100 = raster(chm_100_path)


############
# make dataframes
############

# make dataframe for 50m
# get all the raster-values in a list
backscatter_vectors = lapply(backscatter_50_raster, values)
# make the names
names_backsactter


# make dataframe for 100m


