### Script for Adriano's analysis

### There were two sets of measurements, 
### Dec19/Jan20 and Nov 2020. 
### It would be useful to have drought conditions leading up to the first measurement, 
### and rainfall during the second period (do we have the data up to Nov 2020?)

### For each location, it would be useful to know

### Mean annual rainfall; 
### rainfall in the two years up to January 2020; 
### rainfall Jan 2020 – Nov 2020
### Mean annual water deficit; 
### Water deficit in the two years up to Jan 2020

#######################################################################################
### +++++++++++++++++++++++++++++ General set-up ++++++++++++++++++++++++++++++++++ ###
### clear wk space
rm(list=ls(all=TRUE))

### source all necessary files
source("prepare.R")


###########################################################################################
### +++++++++++++++++++++ Basic code to process the raw data +++++++++++++++++++++++++ ####

### the chosen sites
siteDF <- read_in_sites_and_convert_to_AWAP()


#### 2. Unzip all .z files
####    Only need to run this section of code once
unzip_all_z_files_and_rename(sourceDir = "/Volumes/TOSHIBAEXT/AWAP/rain/2020/")

unzip_all_z_files_Adriano(sourceDir = "/Volumes/TOSHIBAEXT/AWAP/tmax/2020/")

unzip_all_z_files_Adriano(sourceDir = "/Volumes/TOSHIBAEXT/AWAP/vp3pm/2020/")


###########################################################################################
### extract site
extract_selected_sites(sourceDir = "/Volumes/TOSHIBAEXT/AWAP/rain/", 
                       destDir = "input/Adriano",
                       varName = "rain",
                       siteDF = siteDF,
                       user.region.name = "Adriano")

extract_selected_sites(sourceDir = "/Volumes/TOSHIBAEXT/AWAP/tmax/", 
                       destDir = "input/Adriano",
                       varName = "tmax",
                       siteDF = siteDF,
                       user.region.name = "Adriano")

extract_selected_sites(sourceDir = "/Volumes/TOSHIBAEXT/AWAP/vp3pm/", 
                       destDir = "input/Adriano",
                       varName = "vp3pm",
                       siteDF = siteDF,
                       user.region.name = "Adriano")


### 6.1. Calculate saturated vapor pressure based on Tmax
calculate_saturated_vapor_pressure_based_on_Tmax_Adriano(sourceDir = "input/Adriano",
                                                         destDir = "input/Adriano",
                                                         varName = "es",
                                                         user.region.name = "Adriano")

### 6.2. Calculate VPD based on ES and EA
####     Note: check back - VPD range - 30 to + 6, possibly not right !!!!
calculate_VPD_based_on_es_and_vp3pm_Adriano(sourceDir = "input/Adriano",
                                            destDir = "input/Adriano",
                                            varName = "vpd",
                                            user.region.name = "Adriano")


##### 7. Calculate PET minus P
##### 7.1. Calculate PET based on Tmax
#####      return monthly DF
calculate_PET_based_on_Tmax_Adriano(sourceDir = "input/Adriano",
                                    destDir = "input/Adriano",
                                    varName = "pet",
                                    siteDF=siteDF,
                                    user.region.name = "Adriano")

##### 7.2 Calculate PET minus P
#####     return monthly DF 
calculate_PD_based_on_PET_Adriano(sourceDir = "input/Adriano",
                                  destDir = "input/Adriano",
                                  varName = "pd",
                                  siteDF=siteDF,
                                  user.region.name = "Adriano")


### to here. next:
### 1. calculate mean precipitation
### 2. calculate water deficit over the two year period
### Mean annual rainfall; 
### rainfall in the two years up to January 2020; 
### rainfall Jan 2020 – Nov 2020
### Mean annual water deficit; 
### Water deficit in the two years up to Jan 2020


### calculate mean rainfall for each location
siteDF <- calculate_MAP_for_Adirano_locations(siteDF=siteDF)

siteDF <- calculate_MAP_in_2020_for_Adirano_locations(siteDF=siteDF)

siteDF <- calculate_deficit_for_Adriano_locations(siteDF=siteDF)


write.csv(siteDF, "output/Adriano/site_rainfall_Adrinao.csv", row.names=F)
