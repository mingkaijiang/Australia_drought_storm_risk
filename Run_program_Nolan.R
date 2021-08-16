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
nsw.lon.max = 162
nsw.lon.min = 138
nsw.lat.max = -14
nsw.lat.min = -38

nsw.lat.list.s <- seq(nsw.lat.max, (nsw.lat.min+2), by=-2)
nsw.lon.list.s <- seq(nsw.lon.min, (nsw.lon.max-2), by=2)
nsw.lat.list.e <- seq((nsw.lat.max-2), nsw.lat.min, by=-2)
nsw.lon.list.e <- seq((nsw.lon.min+2), nsw.lon.max, by=2)

nswDF <- data.frame("lon.start" = rep(nsw.lon.list.s, each=length(nsw.lat.list.s)),
                    "lon.end" = rep(nsw.lon.list.e, each=length(nsw.lat.list.e)),
                    "lat.start" = rep(nsw.lat.list.s, length(nsw.lon.list.s)),
                    "lat.end" = rep(nsw.lat.list.e, length(nsw.lon.list.e)))


#### 5.1 daily rainfall
for (z in 1:nrow(nswDF)) {
    convert_from_spatial_to_temporal_DF_for_user_defined_regions_rain(sourceDir = "/Volumes/TOSHIBAEXT/AWAP/rain/", 
                                                                      destDir = "input",
                                                                      varName = "rain",
                                                                      user.lat.max = nswDF$lat.start[z],
                                                                      user.lat.min = nswDF$lat.end[z],
                                                                      user.lon.min = nswDF$lon.start[z],
                                                                      user.lon.max = nswDF$lon.end[z],
                                                                      user.region.name = paste0("NSW", z))
}




#### 5.2. daily Tmax
#convert_from_spatial_to_temporal_DF_for_user_defined_regions_tmax(sourceDir = "/Volumes/TOSHIBAEXT/AWAP/tmax/", 
#                                                                  destDir = "input",
#                                                                  varName = "tmax",
#                                                                  user.lat.max = -31,
#                                                                  user.lat.min = -35,
#                                                                  user.lon.max = 153,
#                                                                  user.lon.min = 149,
#                                                                  user.region.name = "SydneyHunter")





##### 7. Calculate PET minus P
##### 7.1. Calculate PET based on Tmax
#####      return monthly DF
#calculate_PET_based_on_Tmax(sourceDir = "input",
#                            destDir = "input",
#                            varName = "pet",
#                            user.lat.max = -31,
#                            user.lat.min = -35,
#                            user.lon.max = 153,
#                            user.lon.min = 149,
#                            user.region.name = "SydneyHunter")
#
##### 7.2 Calculate PET minus P
#####     return monthly DF 
#calculate_PD_based_on_PET(sourceDir = "input",
#                          destDir = "input",
#                          varName = "pd",
#                          user.lat.max = -31,
#                          user.lat.min = -35,
#                          user.lon.max = 153,
#                          user.lon.min = 149,
#                          user.region.name = "SydneyHunter")
