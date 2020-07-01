#############################
# Script to preprocess all Data
# SNAP GRDs
# FNF
# CHM
# COHERENCE
#############################

# The Goal of this script is to have TWO Dataframes at the end
# one for all data in 50m resolution
# one for all data in 100m resolution

# load libraries
library(raster)
library(dplyr)
library(sf)

# define paths
chm_path = "/home/robin/geodata/geo411/GEO411_FSH_Roda/ancillary_data/LAS_diff_Roda_10m_median_NA.tif"
fnf_path = "/home/robin/geodata/geo411/GEO411_FSH_Roda/ancillary_data/FNF/TCD_2015_020m_eu_03035_d05_E40N30/TCD_2015_020m_eu_03035_d05_E40N30.tif"
bs_dir = "/home/robin/geodata/geo411/GEO411_FSH_Roda/snap_grd/final_GRDs_SNAP/"

####################
# Mask the CHM with the FNF MASK
####################

# plot both
