### Script for Rachael Nolan's analysis


#######################################################################################
### +++++++++++++++++++++++++++++ General set-up ++++++++++++++++++++++++++++++++++ ###
### clear wk space
rm(list=ls(all=TRUE))

### source all necessary files
source("prepare.R")


###########################################################################################
### +++++++++++++++++++++ Basic code to process the raw data +++++++++++++++++++++++++ ####

### NSW boundary
nswDF <- prepare_NSW_DFs()

#### 5.1 daily rainfall
for (z in 1:nrow(nswDF)) {
    
    ##### 5.1 daily rainfall
    #convert_from_spatial_to_temporal_DF_for_user_defined_regions_rain_Nolan(sourceDir = "/Volumes/TOSHIBAEXT/AWAP/rain/", 
    #                                                                        destDir = "input",
    #                                                                        varName = "rain",
    #                                                                        user.lat.max = nswDF$lat.start[z],
    #                                                                        user.lat.min = nswDF$lat.end[z],
    #                                                                        user.lon.min = nswDF$lon.start[z],
    #                                                                        user.lon.max = nswDF$lon.end[z],
    #                                                                        user.region.name = paste0("NSW", z))
    #
    ##### 5.2. daily Tmax
    #convert_from_spatial_to_temporal_DF_for_user_defined_regions_tmax_Nolan(sourceDir = "/Volumes/TOSHIBAEXT/AWAP/tmax/", 
    #                                                                        destDir = "input",
    #                                                                        varName = "tmax",
    #                                                                        user.lat.max = nswDF$lat.start[z],
    #                                                                        user.lat.min = nswDF$lat.end[z],
    #                                                                        user.lon.min = nswDF$lon.start[z],
    #                                                                        user.lon.max = nswDF$lon.end[z],
    #                                                                        user.region.name = paste0("NSW", z))
    #
    #
    #
    ###### 7. Calculate PET minus P
    ###### 7.1. Calculate PET based on Tmax
    ######      return monthly DF
    #calculate_PET_based_on_Tmax_Nolan(sourceDir = "input",
    #                                  destDir = "input",
    #                                  varName = "pet",
    #                                  user.lat.max = nswDF$lat.start[z],
    #                                  user.lat.min = nswDF$lat.end[z],
    #                                  user.lon.min = nswDF$lon.start[z],
    #                                  user.lon.max = nswDF$lon.end[z],
    #                                  user.region.name = paste0("NSW", z))
    
    ##### 7.2 Calculate PET minus P
    #####     return monthly DF 
    calculate_PD_based_on_PET_Nolan(sourceDir = "input",
                                    destDir = "input",
                                    varName = "pd",
                                    user.lat.max = nswDF$lat.start[z],
                                    user.lat.min = nswDF$lat.end[z],
                                    user.lon.min = nswDF$lon.start[z],
                                    user.lon.max = nswDF$lon.end[z],
                                    user.region.name = paste0("NSW", z))
}


### checking script
plot_PET(sourceDir <- "input",
         destDir <- "output/Nolan")

##### Calculate water deficit total over period of interest, and its percentil information

##### B6. calculate water deficit (PET - P) drought intex for 2 year period
##### B6.2. water deficit for 2-year period
##### Period of interest: feb-2018 to Jan-2020
compute_water_deficit_percentile_Nolan(sourceDir = "input", 
                                       destDir = "output/Nolan",
                                       duration = "2-year",
                                       nswDF)



#nsw.lon.max = 162
#nsw.lon.min = 138
#nsw.lat.max = -14
#nsw.lat.min = -38
#
#nsw.lat.list.s <- seq(nsw.lat.max, (nsw.lat.min+8), by=-8)
#nsw.lon.list.s <- seq(nsw.lon.min, (nsw.lon.max-8), by=8)
#nsw.lat.list.e <- seq((nsw.lat.max-8), nsw.lat.min, by=-8)
#nsw.lon.list.e <- seq((nsw.lon.min+8), nsw.lon.max, by=8)
#
#nswDF <- data.frame("lon.start" = rep(nsw.lon.list.s, each=length(nsw.lat.list.s)),
#                    "lon.end" = rep(nsw.lon.list.e, each=length(nsw.lat.list.e)),
#                    "lat.start" = rep(nsw.lat.list.s, length(nsw.lon.list.s)),
#                    "lat.end" = rep(nsw.lat.list.e, length(nsw.lon.list.e)))

merge_water_deficit_dataframes_and_plot_Nolan(sourceDir="output/Nolan",
                                              destDir="output/Nolan",
                                              nswDF)

### convert into 3d matrix
#d1 <- sqrt(dim1)
#test <- array(unlist(myData), dim = c(d1,d1,dim2))
#test1 <- test[,,1]
#r1 <- raster(test1)
#plot(r1)
