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
#### takes about 10 hour to run
for (z in 1:nrow(nswDF)) {
    
    ##### 5.1 daily rainfall
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



merge_water_deficit_dataframes_and_plot_Nolan(sourceDir="output/Nolan",
                                              destDir="output/Nolan",
                                              nswDF)

