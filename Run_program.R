############### Risk Assessment of Drought and Storm Risk to Power Grid ###############
################################## Master sript #######################################
### +++++++ code developed by: Mingkai Jiang (m.jiang@westernsydney.edu.au) +++++++ ###
#######################################################################################



#######################################################################################
### +++++++++++++++++++++++++++++ General codes +++++++++++++++++++++++++++++++++++ ###
### clear wk space
rm(list=ls(all=TRUE))

### source all necessary files
source("prepare.R")

###++++++++++++++++++++++++++++++++ End general codes ++++++++++++++++++++++++++++++++####
##########################################################################################


###########################################################################################
### +++++++++++++++ Basic code to generate climate extreme index +++++++++++++++++++++ ####
#### Structure:
#### 0. Download data
#### 1. Process raw data 
#### 2. Calculate climate extremes with alternative ways
####    a. drought index - duration (count number of days with no or little rainfall)
####    b. storm index - rainfall intensity over one / three / five / ten days after drought
### 0 . Download AWAP data from BOM website
#download_AWAP_rainfall_data(destDir="/Volumes/TOSHIBAEXT/AWAP/rain/")

#### 1. Unzip all .z files
####    Only need to run this code once
#unzip_all_z_files(s.yr = 1900, e.yr = 2018)

#### Convert from per day to per grid
#### Only need to run this code once
#convert_from_spatial_to_temporal_DF(sourceDir = "/Volumes/TOSHIBAEXT/AWAP/rain/", 
#                                    destDir = "/Volumes/TOSHIBAEXT/AWAP/output")



#### 2. Calculate storm index, based on daily data;
#### For each grid, merge all daily data,
#### Then compute distribution of rainfall
#### Then get the extreme rainfall threshold
compute_storm_index(sourceDir = "input", 
                    destDir = "output",
                    duration = "1-day")



#### 3. Calculate drought index:
#### For each grid, merge all daily data,
#### Then count number of days with no rainfall (or very limited rainfall)
compute_drought_index(sourceDir = "input", 
                      destDir = "output",
                      duration = "1-year")



#### 4. For each extreme rainfall event, obtain number of droughted days before the rainfall event
compute_drought_and_storm_event_severity(sourceDir = "input", 
                                         destDir = "output",
                                         storm.duration = "1-day",
                                         drought.duration = "1-year")


#### 5. Investigate how extreme the drought is before the storm event
check_drought_extremeness_before_storm_event()


#### 6. Make spatial plots



#### 7. Make statistics

### +++++++++++++++ End basic code to generate climate extreme index ++++++++++++++++++ ####
############################################################################################

