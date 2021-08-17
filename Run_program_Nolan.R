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

nsw.lat.list.s <- seq(nsw.lat.max, (nsw.lat.min+8), by=-8)
nsw.lon.list.s <- seq(nsw.lon.min, (nsw.lon.max-8), by=8)
nsw.lat.list.e <- seq((nsw.lat.max-8), nsw.lat.min, by=-8)
nsw.lon.list.e <- seq((nsw.lon.min+8), nsw.lon.max, by=8)

nswDF <- data.frame("lon.start" = rep(nsw.lon.list.s, each=length(nsw.lat.list.s)),
                    "lon.end" = rep(nsw.lon.list.e, each=length(nsw.lat.list.e)),
                    "lat.start" = rep(nsw.lat.list.s, length(nsw.lon.list.s)),
                    "lat.end" = rep(nsw.lat.list.e, length(nsw.lon.list.e)))


#### 5.1 daily rainfall
z=1
sourceDir = "/Volumes/TOSHIBAEXT/AWAP/rain/"
destDir = "input"
varName = "rain"
user.lat.max = nswDF$lat.start[z]
user.lat.min = nswDF$lat.end[z]
user.lon.min = nswDF$lon.start[z]
user.lon.max = nswDF$lon.end[z]
user.region.name = paste0("NSW", z)

for (z in 1:nrow(nswDF)) {
    
    #### 5.1 daily rainfall
    convert_from_spatial_to_temporal_DF_for_user_defined_regions_rain_Nolan(sourceDir = "/Volumes/TOSHIBAEXT/AWAP/rain/", 
                                                                            destDir = "input",
                                                                            varName = "rain",
                                                                            user.lat.max = nswDF$lat.start[z],
                                                                            user.lat.min = nswDF$lat.end[z],
                                                                            user.lon.min = nswDF$lon.start[z],
                                                                            user.lon.max = nswDF$lon.end[z],
                                                                            user.region.name = paste0("NSW", z))
    
    #### 5.2. daily Tmax
    convert_from_spatial_to_temporal_DF_for_user_defined_regions_tmax_Nolan(sourceDir = "/Volumes/TOSHIBAEXT/AWAP/tmax/", 
                                                                            destDir = "input",
                                                                            varName = "tmax",
                                                                            user.lat.max = nswDF$lat.start[z],
                                                                            user.lat.min = nswDF$lat.end[z],
                                                                            user.lon.min = nswDF$lon.start[z],
                                                                            user.lon.max = nswDF$lon.end[z],
                                                                            user.region.name = paste0("NSW", z))
    
    
    
    ##### 7. Calculate PET minus P
    ##### 7.1. Calculate PET based on Tmax
    #####      return monthly DF
    calculate_PET_based_on_Tmax_Nolan(sourceDir = "input",
                                      destDir = "input",
                                      varName = "pet",
                                      user.lat.max = nswDF$lat.start[z],
                                      user.lat.min = nswDF$lat.end[z],
                                      user.lon.min = nswDF$lon.start[z],
                                      user.lon.max = nswDF$lon.end[z],
                                      user.region.name = paste0("NSW", z))
    
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


#### Merge
merge_and_compute_NSW_P_PET_index(sourceDir = "input",
                                  destDir = "output",
                                  duration = "1-year",
                                  n=nrow(nswDF))


#### B6. calculate water deficit (PET - P) drought intex for antecedent 1 and 2 year period

#### B6.1. water deficit for 1-year period
compute_antecedent_water_deficit_for_user_defined_regions(sourceDir = "input", 
                                                          destDir = "output/antecedent_water_deficit",
                                                          user.region.name = "SydneyHunter",
                                                          duration = "1-year")

##### B6.2. water deficit for 2-year period
compute_antecedent_water_deficit_for_user_defined_regions(sourceDir = "input", 
                                                          destDir = "output/antecedent_water_deficit",
                                                          user.region.name = "SydneyHunter",
                                                          duration = "2-year")
