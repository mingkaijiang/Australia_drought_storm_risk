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
### +++++++++++++++++++++ Basic code to process the raw data +++++++++++++++++++++++++ ####
#### Structure:
#### 1. Download data
#### 2. Unzip files
#### 3. Process raw data 

#### 1 . Download AWAP data from BOM website
####     Only need to run this code once.
#download_AWAP_rainfall_data(destDir="/Volumes/TOSHIBAEXT/AWAP/rain/")

#### 2. Unzip all .z files
####    Only need to run this code once
#unzip_all_z_files(s.yr = 2018, e.yr = 2020)

#### 3. Convert from per day to per grid
#### Only need to run this code once
#convert_from_spatial_to_temporal_DF(sourceDir = "/Volumes/TOSHIBAEXT/AWAP/rain/", 
#                                    destDir = "/Volumes/TOSHIBAEXT/AWAP/output")

#### make user specified selection of spatial range
#### Note that, region has to be small (i.e. ~ 10 by 10 degree) to not exceed memory limit
#### User also need to specify region name.
convert_from_spatial_to_temporal_DF_for_user_defined_regions(sourceDir = "/Volumes/TOSHIBAEXT/AWAP/rain/", 
                                                             destDir = "/Volumes/TOSHIBAEXT/AWAP/output",
                                                             user.lat.max = -28,
                                                             user.lat.min = -36,
                                                             user.lon.max = 155,
                                                             user.lon.min = 145,
                                                             user.region.name = "Larger_Sydney")

###########################################################################################
### +++++++++++++++ Basic code to generate climate extreme index +++++++++++++++++++++ ####
#### Structure:
#### 1. Calculate storm index
#### 2. Calculate drought index
#### 3. Calculate drought and storm severity index


#### 1. Calculate storm index, based on Sydney region daily data;
####     Storm index has duration options of 1 - 5 days
####     Output a 3 dimension matrix with lat lon and 9 layers of storm index
####     Each layer is the 99.9th, 99th, 95th, 90th, 80th, 70th, 60th, 50th, 40th percentile
compute_storm_index(sourceDir = "input", 
                    destDir = "output",
                    user.region.name = "Larger_Sydney",
                    duration = "1-day")



#### 2. Calculate drought index:
####     Drought index has duration options of consecutive no rain days, 1-year, 2-year,
####     Output a 3 dimension matrix with lat lon and 9 layers of storm index
####     Each layer is the number of no rain days, 
####     0.1th, 1th, 5th, 10th, 20th, 30th, 40th, 50th percentile of the rainfall distribution
compute_drought_index(sourceDir = "input", 
                      destDir = "output",
                      user.region.name = "Larger_Sydney",
                      duration = "1-year")



#### 3. For each extreme rainfall event, obtain the drought severity information
####    User can specify a particular date.of.interest,
####    the script will calculate the short storm and aggregated rainfall data 
####    based on user-defined storm and drought duration,
####    then compare against the all-time extreme
####    to indicate the severity of the storm and drought event.
####    User-defined storm.duration can be: 1 - 5 days
####    User-defined drought.duration can be: 1 and 2 years
####    The script will run over the region defined in the inFile file.
####    Output includes: table of short-term rainfall and long-term rainfall intensity
####                     table of storm and drought severity
compute_drought_and_storm_event_severity(sourceDir = "input", 
                                         destDir = "output",
                                         user.region.name = "Larger_Sydney",
                                         date.of.interest = "20191126",
                                         storm.duration = "1-day",
                                         drought.duration = "1-year")


#### 4. Make spatial plots
sourceDir = "output"
destDir = "output/plots"
user.region.name = "Larger_Sydney"
date.of.interest = "20191126"
storm.duration = "1-day"
drought.duration = "1-year"
user.lat.max = -28
user.lat.min = -36
user.lon.max = 155
user.lon.min = 145

make_spatial_plots(sourceDir = "output",
                   destDir = "output/plots",
                   user.region.name = "Larger_Sydney",
                   date.of.interest = "20191126",
                   user.lat.max = -28,
                   user.lat.min = -36,
                   user.lon.max = 155,
                   user.lon.min = 145,
                   storm.duration = "1-day",
                   drought.duration = "1-year")



### +++++++++++++++ End basic code to generate climate extreme index ++++++++++++++++++ ####
############################################################################################


### to do list:
### 1. delete grids with mean annual precipitation == 0; create a polygon to offset these regions;
### 2. Check of values make sense for greater Sydney region (high precipitation), better color scheme to distinguish the bins;
### 3. Possibly create extreme index for the entire Australia 
###    shouldn't be big, because lon x lat x extreme percentile, based on one particular duration threshold
###    hence we will have several files for each extreme index, defined by duration, 
###    then in each file we will have 3d matrix with the last dimension splitting into different extreme percentile.
### 4. With Australia extreme index created, we can run a subregion, 
###    to check the severity of a particular storm x drought event for a particular period.
