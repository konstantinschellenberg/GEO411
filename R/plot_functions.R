#' Function definition for plotting coherence & backscatter with the
#' Canopy Height Model (CHM). Makes use of ggplot-functions
#' 13-06-2020, Konstantin Schellenberg

scatterplot = function(ras, chm, bins){
    # coerch both rasters to dataframes
    df = data.frame(ras = values(ras), chm = values(chm))

    # Raster Scatterplot
    ggplot(df, aes(x = ras, y = chm)) +
        geom_bin2d(bins = bins) +
        scale_fill_continuous(type = "viridis") +
        ggtitle("ALOS-2 HV Coherence for 2015-08-21/2015-10-02 over Roda")+
        xlab("Coherence") +
        ylab("Canopy Height [m]") +
        theme_classic()
}

# 2D Density map
# ggplot(df, aes(x = ras, y = chm)) +
#     stat_density_2d(aes(fill = ..density..), geom = "raster", contour = T) +
#     scale_x_continuous(expand = c(0, 0)) +
#     scale_y_continuous(expand = c(0, 0))
