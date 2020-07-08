# load libraries
library(ggplot2)

# read data
metadata = read.csv("/home/robin/Desktop/metadata.csv")

# ggplot
ggplot(metadata) + geom_point(aes(x = Date, y = Val, color = factor(Date))) + facet_grid(vars(Param), scales = "free")
