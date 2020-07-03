# Processing

library(raster)
library(tidyverse)
library(data.table)
library(ggplot2)
library(sf)

options(max.print = 200, scipen = 100)

# set environment to "model"
env = "D:/Geodaten/Master/projects/GEO411/"
setwd(env)

# LOAD DATA --------------------------------------------------------------------

# Load 50m rasters
chm50 = raster("data/ancillary/chm_50m_maskedtofnf_32632.tif")
chm100 = raster("data/ancillary/chm_100m_maskedtofnf_32632.tif")

bsc50_path = list.files("data/BSC", recursive = TRUE, pattern = "32632_50m", full.names = TRUE)
bsc100_path = list.files("data/BSC", recursive = TRUE, pattern = "32632_10m", full.names = TRUE)
coh50_path = list.files("data/COH", recursive = TRUE, pattern = "32632_50", full.names = TRUE)
coh100_path = list.files("data/COH", recursive = TRUE, pattern = "32632_10", full.names = TRUE)

list_path50 = c(bsc50_path, coh50_path)
list_path100 = c(bsc100_path, coh100_path)

list_raster50 = vector(mode = "list", length = length(unlist(list_path50)))
list_raster100 = vector(mode = "list", length = length(unlist(list_path100)))
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

# RESAMPLE to extent intersecting with all data
# not necessary
rasters50 = lapply(unlist(list_path50), function(x){
    raster(x) %>% resample(dummy_raster50)
})

rasters100 = lapply(unlist(list_path100), function(x){
    raster(x) %>% resample(dummy_raster100)
})

chm50 = chm50 %>% resample(dummy_raster50)
chm100 = chm100 %>% resample(dummy_raster100)

list_chm_backscatter_coherence50 = do.call(c, list(chm50, rasters50))
list_chm_backscatter_coherence100 = do.call(c, list(chm100, rasters100))

# Make DataFrame ---------------------------------------------------------------

list_all = list(list_chm_backscatter_coherence50, list_chm_backscatter_coherence100)
df_all = vector("list", length = length(list_all))
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
    df_all[[h]] = final_df
}

# rename the master lists
names(df_all) = c("res_50", "res_100")

if (!file.exists("model/dataframes/final_df.RDS")){
    saveRDS(df_all, "model/dataframes/final_df.RDS")
}

# load dataframe
df50 = readRDS("model/dataframes/final_df.RDS")[[1]]
df100 = readRDS("model/dataframes/final_df.RDS")[[2]]

# IMPUTE -----------------------------------------------------------------------
# throw out all lines where there is one NA (not sure if this is the best way to impute data;)

dfs = list(df50 = df50, df100 = df100)

for (i in seq_along(dfs)){
    df = dfs[[i]]
    df = df[complete.cases(df),]

    # still 0s inside, gdalwarp: dstnodata = 0 !!

    # rename colnames
    names(df) = names
    dfs[[i]] = df
}

# CATEGORISATION ---------------------------------------------------------------
#' Aim is to assign incrementing categories of CH for boxplot of variable relationship

for (i in seq_along(dfs)){
    df = dfs[[i]]
    df = dfs[[1]] %>% as.data.table()

    breaks = seq(floor(min(df[,chm])), floor(max(df[,chm])))
    labels = as.character(breaks)
    labels = labels[1:length(labels) - 1] # remove last entry

    df[, height:= cut(chm, breaks = breaks, labels = labels)]

    # df[, table(height)]
    # df %>% select(chm, height) %>% arrange(height)
    dfs[[i]] = df
}

# BOXPLOT ----------------------------------------------------------------------

# get one dataframe
df = dfs[[1]]

ggplot(df, aes(height, bs_150821)) +
    geom_boxplot()

par(mfrow = c(3,3))

