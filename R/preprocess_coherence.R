#' Coherence data wragling
#' Konstantin Schellenberg, 13.06.2020
#'
#' #!/usr/bin/env Rscript

library("optparse", quietly = T)

option_list = list(
    make_option(c("-d", "--directory"), type="character", default=NULL,
                help="Robin's directory tree", metavar="character"),
    make_option(c("-l", "--lidar"), type="character", default=NULL,
                help="lidar file", metavar="character"),
    make_option(c("-t", "--targetresolution"), type="integer", default="30",
                help="target resolution [default= %default]", metavar="integer")
)

opt_parser = OptionParser(option_list=option_list)
opt = parse_args(opt_parser)

# if (is.null(opt$file)){
#     print_help(opt_parser)
#     stop("At least one argument must be supplied", call.=FALSE)
# }

# For data fetching ------------------------------------------------------------
# get all geocoded coherences in Robin's folder structure before running this script
# delete cropped_topophase_cor_geo files!

# User Input here

# For manually running the script
data_dir = "D:/Geodaten/GEO411/01_data"
lidar_height = "D:/Geodaten/GEO411/01_data/chm_maskedtofnf_30_32632_-99nodata.tif"
target_resolution = c(30, 30)

data_dir = opt$directory
lidar_height = opt$lidar
target_resolution = c(rep(opt$targetresolution, 2))

cat("\ndata directory: \n", data_dir, "\n\n")
cat("lidar height file: \n", lidar_height,"\n\n")
# cat("target resolution: ", target_resolution[1])

# ------------------------------------------------------------------------------

#' you want to delete all files created by this script? Urge this:
#' for R-regex info:
#' http://www.endmemo.com/r/grep.php

l = list.files(path = data_dir, full.names = T, recursive = T)
coherence_files = l[grepl("topophase.cor.geo", x = l)]
coherence_files_created = coherence_files[!grepl("vrt$|xml$|topophase.cor.geo$", x = coherence_files)]
# print(coherence_files_created)
file.remove(coherence_files_created) # <------- run this for deletion - be careful!

# ------------------------------------------------------------------------------
#' Functionality of the script:
#' Phase 1 = Extraction band 1 from ISCE topophase.geo.cor.vrt
#' Phase 2 = Reprojection to EPSG:32632
#' Phase 3 = Resampling to custom [m] resolution and value 0 = NA
#' Phase 4 = Cropping Coherence to lidar, resampling to lidar mesh

library(gdalUtils, quietly = T)
library(raster, quietly = T)

# No user movements here -------------------------------------------------------

l = list.files(path = data_dir, full.names = T, recursive = T)
path_phase1 = l[grepl("topophase.cor.geo.vrt$", l)]

# Phase 1 - Extraction band 1 from ISCE topophase.geo.cor.vrt ------------------

extr = function(ras){

    outfile = c()
    # create new output names
    for(names in ras){
        sub = gsub(pattern = "geo.vrt", replacement = "geo.tif", x = names)
        outfile = append(outfile, sub)
    }

    # trying gdal_translate for getting band 2 (coherence)
    tryCatch(
        expr = for(i in 1:length(ras)){
            gdalUtils::gdal_translate(src_dataset = ras[i], dst_dataset = outfile[i], b = 2)
            cat("Band extraction:", outfile[i], "\n", sep = "\n")
        },
        warning = function(i){cat("Band extraction failed or has been already successfully executed, with\n\n")}
    )
}

extr(path_phase1)

# Phase 2 - Reprojection to EPSG:32632 -------------------------------------------------------------------

l = list.files(path = data_dir, full.names = T, recursive = T)
path_phase2 = l[grepl("topophase.cor.geo.tif$", l)]

proj = function(ras){

    outfile = c()
    # create new output names
    for(names in ras){
        sub = gsub(pattern = "geo.tif", replacement = "geo.proj.tif", x = names)
        outfile = append(outfile, sub)
    }

    # trying gdalwarp for reprojecting rasters to EPSG:32632
    tryCatch(
        expr = for(i in 1:length(ras)){
            gdalUtils::gdalwarp(srcfile = ras[i], dstfile = outfile[i], t_srs = "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs", overwrite = TRUE)
            cat("Reprojected rasters created:", outfile[i], "\n", sep = "\n")
        },
        warning = function(i){
            file.rem
            cat("Reprojection failed or has been already successfully executed, with\n\n")}
    )
}

proj(path_phase2)

# Phase 3 - Resampling to custom [m] resolution and value 0 = NA ---------------

l = list.files(path = data_dir, full.names = T, recursive = T)
path_phase3 = l[grepl("topophase.cor.geo.proj.tif$", l)]

resamp = function(ras){
    #' trying gdal_translate
    #' 1. Extracting band 1 only

    # create new output names
    outfile = c()
    for(names in ras){
        sub = gsub(pattern = "geo.proj.tif", replacement = "geo.proj.resamp.na.tif", x = names)
        outfile = append(outfile, sub)
    }

    tryCatch(
        expr = for(i in 1:length(ras)){
            gdalUtils::gdal_translate(src_dataset = ras[i], dst_dataset = outfile[i], tr = target_resolution, r = c("nearest"), a_nodata = 0)
            cat("Reprojected rasters created:", outfile[i], "\n", sep = "\n")
        },
        warning = function(i){cat("Resampling failed or has been already successfully executed, with\n\n")}
    )
}

resamp(path_phase3)

# final check, weather all data is right in place. topophase.cor.geo.proj.resamp.na.tif must exist
l = list.files(path = data_dir, full.names = T, recursive = T)
path_phase4 = l[grepl("topophase.cor.geo.proj.resamp.na.tif$", l)]

# Phase 4 - Cropping Coherence to lidar, resampling to lidar mesh --------------
# done with raster-pkg

# load CHM
chm_original = raster(lidar_height)

# create mask to which the data is resampled
mask = raster(ext = extent(chm_original), resolution = target_resolution, crs = "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs")

# resampling chm
chm = resample(chm_original, mask)

resamp_lidar = function(ras, lidar){

    outfile = c()
    for(names in ras){
        sub = gsub(pattern = "geo.proj.resamp.na.tif", replacement = "geo.proj.resamp.na.crop.tolidar.tif", x = names)
        outfile = append(outfile, sub)
    }

    # resample coherence to canopy height
    for(i in 1:length(ras)){
        loaded.ras = raster(ras[i])
        resample(x = loaded.ras, y = chm,
                 filename = outfile[i], overwrite = TRUE)
    }
}

resamp_lidar(path_phase4, chm)

cat("Final Coherences stored on disk:\n\n")
print(l[grepl("topophase.cor.geo", x = l)])
