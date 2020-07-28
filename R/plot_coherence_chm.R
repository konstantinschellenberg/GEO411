#' Scatterplot of lidar canopy height and InSAR coherence and
#' init: 24.05.2020

library(raster)
library(ggplot2)
library(tidyverse)
library(gridExtra)
library(grid)

# mother directory, as specified by robin
path_dir = "D:/Geodaten/Master/projects/GEO411"
plot_dir = "D:/Geodaten/Master/projects/GEO411/model/plots/"

setwd(path_dir)

# LOAD DATA --------------------------------------------------------------------

# get data
coh = list.files("data/COH", full.names = T ,recursive = T)
bsc = list.files("data/BSC", full.names = T ,recursive = T)

# get 50m resolution
pre_processed_coherences = coh[grepl("32632_10", coh)]
pre_processed_backscatter = bsc[grepl("32632_32632_10m", bsc)]

chm = raster("D:/Geodaten/Master/projects/GEO411/data/ancillary/chm_100m_maskedtofnf_32632.tif") %>%
    `names<-`("chm")

fnf = raster("data/ancillary/chm_100m_maskedtofnf_32632.tif")
fnf = projectRaster(fnf, chm, res = c(100, 100), method = "nearest")

extent(fnf)
extent(chm)


# variable names
spl = strsplit(pre_processed_coherences, .Platform$file.sep)
coh_names = paste0(sapply(spl, function(x){name = x[[length(x)]]; substr(name, 1, 8)}), "_ALOS2_coherence")

spl = strsplit(pre_processed_backscatter, .Platform$file.sep)
bsc_names = paste0(sapply(spl, function(x){name = x[[length(x)]]; substr(name, start = 1, stop = 6) %>% paste0("20", .)}), "_ALOS2_backscatter")

coh = map2(pre_processed_coherences, coh_names, ~ raster(.x) %>% `names<-`(.y))
bsc = map2(pre_processed_backscatter, bsc_names, ~ raster(.x) %>% `names<-`(.y))

# load prediction
pred = raster("model/RF/Predicion_100m.tif")

# resample to chm
pred = resample(pred, chm)

# apply FNF mask
pred = mask(pred, fnf)

# Histogram --------------------------------------------------------------------
# Histogram creation for CHM
h = hist(chm, breaks = 50, maxpixels = 500000)
h$counts = h$counts
h_data1 = data.frame(breaks = h$mids, counts = h$counts)

# hist plotting
g1 = ggplot(h_data, aes(x = breaks, y = counts)) +
    geom_bar(stat = "identity", fill='darkgreen', alpha = 0.8, width = .5) +
    ggtitle("Lidar Canopy Height Distribution in Roda Catchment") +
    xlab("Canopy Height [m]")+
    ylab("Area [ha]") +
    theme_classic()

# ggsave(plot = g1, filename = "Canopy Height Histogram.png", path = plot_dir, device = "png", width = 5, height = 3)

h = hist(pred, breaks = 25, maxpixels = 500000)
h$counts = h$counts
h_data2 = data.frame(breaks = h$mids, counts = h$counts)

# hist plotting
g2 = ggplot() +
    geom_bar(aes(x = h_data1$breaks, y = h_data1$counts), stat = "identity", fill='darkgreen', alpha = 0.5, width = .5) +
    geom_bar(aes(x = h_data2$breaks, y = h_data2$counts), stat = "identity", fill='red', alpha = 0.5, width = .5) +
    ggtitle("Prediction Height Distribution in Roda Catchment") +
    xlab("Lidar Height [m] (green); Predicted Height [m] (red)")+
    ylab("Area [ha]") +
    theme_classic()

ggsave(plot = g2, filename = "Prediction Height Histogram.png", path = plot_dir, device = "png", width = 5, height = 3)


# Scatterplot ------------------------------------------------------------------
# Creating cross scatterplots for coherence and CHM for 1 date


#################################
# COHERENCE
#################################

# basic plotting
# plot(values(coh[[1]]), values(chm))

# plot function
scatterplot = function(x, y, bins, xlab, ylab, title){

    library(raster)

    # coerce both rasters to dataframes
    df = data.frame(x = raster::values(x), y = raster::values(y))

    # Raster Scatterplot
    ggplot(df, aes(x = x, y = y)) +
        geom_bin2d(bins = bins) +
        scale_fill_continuous(type = "viridis") +
        ggtitle(title)+
        ylab(ylab) +
        xlab(xlab) +
        theme(legend.position = "none")
    theme_classic()
}


arrange_plot = function(stack, chm, names, xlab, ylab, toptitle, nrow, gridtext){

    plots = vector("list", nlayers(stack))
    for (i in 1:nlayers(stack)){
        plots[[i]] = scatterplot(chm, stack[[i]], bins = 100,
                                 xlab = xlab, ylab = ylab, title  = names)
    }

    grid.arrange(grobs = plots, nrow = nrow, top = toptitle,
                 bottom = grid::grid.text(gridtext, gp = gpar(fontface = 3, fontsize = 9, hjust = 1, x = 1)))
}

arrange_plot(coh, chm, coh_names, xlab = "Canopy Height [m]", ylab = "Coherence", toptitle = "ALOS-2 HV Coherence",
             nrow = 3, gridtext = "Interferograms processed by ISCE") %>%
    ggsave(filename = "Coherence_HV-CHM.png", path = plot_dir, limitsize = F, width = 3, height = 9)

arrange_plot(bsc, chm, bsc_names, xlab = "Canopy Height [m]", ylab = "Backscatter coefficient s0", toptitle = "ALOS-2 HV Backscatter",
             nrow = 3, gridtext = "Processed by SNAP") %>%
    ggsave(filename = "Backscatter_HV-CHM.png", path = plot_dir, limitsize = F, width = 7, height = 9)

arrange_plot(bsc.db, chm, bsc.db_names, xlab = "Canopy Height [m]", ylab = "Backscatter coefficient [db]", toptitle = "ALOS-2 HV Backscatter",
             nrow = 3, gridtext = "Processed by SNAP") %>%
    ggsave(filename = "Backscatter_HVdb-CHM.png", path = plot_dir, limitsize = F, width = 7, height = 9)
