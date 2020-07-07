# load libraries
library(raster)
library(ggplot2)

# load data
bs = raster("/home/robin/geodata/geo411/GEO411_FSH_Roda/snap_grd/backscatter_resampled_50_100m/150821_32632_32632_10m.tif")
chm = raster("/home/robin/uni/semester2/geo411/git/GEO411/data/ancillary/chm_100m_maskedtofnf_32632.tif")

# plot them
plot(bs)
plot(chm, add = T)

# make it a dataframe
df = data.frame(bs = values(bs), chm = values(chm))

# count na
lapply(df, function(x) sum(is.na(x)))

# make scatterplot
# Raster Scatterplot
ggplot(df, aes(x = chm, y = bs)) +
    geom_bin2d(bins=100) +
    scale_fill_continuous(type = "viridis") +
    ggtitle("")+
    xlab("") +
    ylab("") +
    ylim(0,0.3) +
    theme_classic()
