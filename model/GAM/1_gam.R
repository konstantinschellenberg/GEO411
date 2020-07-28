################
# GAMs for predicting forest Height from Backscatter and Coherence
################

# load libraries
library(mgcv)
library(raster)
library(dplyr)
library(ggplot2)

# CRS
crs = "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs"

# LOAD DATA --------------------------------------------------------------------
trainingtestset = readRDS("model/dataframes/trainingtestset.RDS")
predictionset = readRDS("model/dataframes/predictionset.RDS")

###########
##  EDA  ##
##########

# make melted df (resolution as one variable in a column)
melteddf = function(trainingtestset, param, date=1){
    # determine dimension of final dataframe
    # where is everywhere the param?
    c = grep(param, names(trainingtestset[[1]]))
    # index of this one
    if(c!=1){
        j = grep(param, names(trainingtestset[[1]]), value = T) %>% grep(date,.)
        }else{
            j = 1
            }
    # which of the indexes with the right param is the correct one
    f = c[j]
    # filter
    l = trainingtestset[[1]][f]
    l = nrow(l)
    # initalize dataframe
    df_final = data.frame(matrix(NA, ncol = 2, nrow = l))
    cat(dim(df_final))
    # get resolutions that will be as factor in the column later
    names_resolutions = names(trainingtestset)
    #filter columns that include param
    for (i in seq_along(trainingtestset)){
        if(i==1){
        names_i = names(trainingtestset[[i]])
        param_index = grep(param, names_i, value = T)
        if(param_index=="chm"){
            j = 1
        }else{
            j = grep(param, names_i, value = T) %>% grep(date, .)
            j = param_index[j]
        }
        vals =  trainingtestset[[i]][j]
        res_fac = as.factor(names_resolutions[[i]])
        df_final[,1] = vals
        df_final[,2] = res_fac

        }else{
            names_i = names(trainingtestset[[i]])
            param_index = grep(param, names_i, value = T)
            # filter dataframe in list
            if(param_index=="chm"){
                j = 1
            }else{
                j = grep(param, names_i, value = T) %>% grep(date,.)
                j = param_index[j]
            }
            vals = trainingtestset[[i]][j]
            df = data.frame(matrix(NA, ncol = 2, nrow = nrow(vals)))
            df[,1] = vals
            df[,2]= as.factor(names_resolutions[[i]])
            names(df) = names(df_final)
            df_final = rbind(df_final, df)
        }
        names(df_final) = c("val", "res")
    }
    return(df_final)
}

plotdis = function(param, date){
    names = names(trainingtestset[[1]])
    if("bs" %in% param){
        p = "Backscatter"
    }else if("coh" %in% param){
        p = "Coherence"
    }else{
        p = "CHM"
    }
    date = grep(date, names, value = T)
    if("bs" %in% param){
        date = substr(date, 4, nchar(date))
    }else if("coh" %in% param){
        date = substr(date,5,nchar(date))
    }else{
        date = "Lidar"
    }
    a = melteddf(trainingtestset, param, date)
    ggplot(a) + geom_density(aes(val, color=res), alpha=.4) + ggtitle(sprintf("Distribution of %s \n %s", p, date)) +
        theme_minimal() + xlab(p) + ylab("Density")
}

# show distributions for different resolutions of backscatter on day 19 of Feb 2016
plotdis("chm", "1")

#######################
##  CHM ~ Coherence  ##
#######################

# 50m
df50_coh = trainingtestset$df50 %>% select(contains("coh") | contains("chm"))
df50_coh$res = factor(50)
# 50m
df100_coh = trainingtestset$df100 %>% select(contains("coh") | contains("chm"))
df100_coh$res = factor(100)
# make one dataframe and melt it
df_all = rbind(df50_coh, df100_coh)
ggplot(data = df_all) + geom_point(aes(x = chm, y = coh_15_15, color=factor(res)), alpha=.04) + ggtitle("CHM VS Coherence (50m and 100m)")


###########
##  GAM  ##
###########

gam_1 = gam(chm ~ s(coh_15_15) + s(coh_15_16) + s(coh_18_18), data = dfs$df50)


#**********************************************************
# 5 APPLY SPATIAL PREDICTIONS TO GRIDDED DATA -------------
#**********************************************************

##
# Convert Coherence Dataframes back to raster
##

# read prediction dataset
pred_coh = prediction$df50 %>% select(contains("coh") | contains("x") | contains("y"))
# reorder the columns
xy = grep("x|y", names(pred_coh))
pred_coh_new = pred_coh[-xy]
pred_coh_new = cbind(pred_coh[xy], pred_coh_new)

# make raster from dataframe
coh_raster_list = vector(mode="list", length = 3)
for(i in seq_along(pred_coh)){
    if(i != 1 & i!= 2){
        print(i-2)
        print(names(pred_coh)[[i-2]])
        r = rasterFromXYZ(pred_coh_new[c(1,2,i)], res = c(50,50), crs="+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs")
        coh_raster_list[[i-2]] = r
    }
}
# stack the covariates
s = stack(coh_raster_list)
pred_gam = raster::predict(s, gam_1, type="response")
plot(pred_gam, main="CHM regressen onto Cohrence with GAMs [50m]")

