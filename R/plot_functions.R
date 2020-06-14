#' Function definition for plotting coherence & backscatter with the
#' Canopy Height Model (CHM). Makes use of ggplot-functions
#' 13-06-2020, Konstantin Schellenberg

scatterplot = function(x, y, bins, xlab, ylab, title){

    # coerch both rasters to dataframes
    df = data.frame(x = values(x), y = values(y))

    # Raster Scatterplot
    ggplot(df, aes(x = x, y = y)) +
        geom_bin2d(bins = bins) +
        scale_fill_continuous(type = "viridis") +
        ggtitle(title)+
        ylab(ylab) +
        xlab(xlab) +
        theme_classic()
}

# 2D Density map
# ggplot(df, aes(x = x, y = y)) +
#     stat_density_2d(aes(fill = ..density..), geom = "raster", contour = T) +
#     scale_x_continuous(expand = c(0, 0)) +
#     scale_y_continuous(expand = c(0, 0))
