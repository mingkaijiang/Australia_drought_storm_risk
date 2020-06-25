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
#### 3. Plot year 2019 and check with observation
#### 4. Convert data from per day to per grid, for whole Australia
#### 5. Convert data from per day to per grid, for user defined regions

#### 1 . Download AWAP data from BOM website
####     Only need to run this code once.
#download_AWAP_rainfall_data(destDir="/Volumes/TOSHIBAEXT/AWAP/rain/")

#### 2. Unzip all .z files
####    Only need to run this code once
#unzip_all_z_files(s.yr = 2018, e.yr = 2020)

#### 3. Plot one-year total rainfall to check rmatches with BOM observations
#plot_total_rainfall_for_a_year(sourceDir = "/Volumes/TOSHIBAEXT/AWAP/rain/", 
#                               destDir = "output",
#                               user.defined.year = 2019)

#### 4. Convert from per day to per grid
#### Only need to run this code once
#### For whole Australia
#### only need to run once, takes long to run (2 days)
#convert_from_spatial_to_temporal_DF_whole_Australia(sourceDir = "/Volumes/TOSHIBAEXT/AWAP/rain/", 
#                                                    destDir = "/Volumes/TOSHIBAEXT/AWAP/output")


#### 5. make user specified selection of spatial range
#### Note that, region has to be small (i.e. ~ 10 by 10 degree) to not exceed memory limit
#### User also need to specify region name.
#### Only need to run once, takes long to run (2 hour)
### Sydney
#convert_from_spatial_to_temporal_DF_for_user_defined_regions(sourceDir = "/Volumes/TOSHIBAEXT/AWAP/rain/", 
#                                                             destDir = "/Volumes/TOSHIBAEXT/AWAP/output",
#                                                             user.lat.max = -28,
#                                                             user.lat.min = -36,
#                                                             user.lon.max = 155,
#                                                             user.lon.min = 145,
#                                                             user.region.name = "Larger_Sydney")


###########################################################################################
### +++++++++++++++ Basic code to generate climate extreme index +++++++++++++++++++++ ####
#### Structure:
#### 1. Calculate storm index
#### 2. Calculate drought index
#### 3. Calculate wind index

#### 1. Calculate storm index, based on Sydney region daily data;
####     Storm index has duration options of 1 - 5 days
####     Output a 3 dimension matrix with lat lon and 9 layers of storm index
####     Each layer is the 99.9th, 99th, 95th, 90th, 80th, 70th, 60th, 50th, 40th percentile

#### placeholder for computing storm index for whole Australia (merging 23 rds data)
#merge_and_compute_Australia_storm_index(sourceDir = "/Volumes/TOSHIBAEXT/AWAP/output",
#                                        destDir = "/Volumes/TOSHIBAEXT/AWAP/output",
#                                        duration = "1-day")


#### Generate storm index for user defined region, 
#### based on user defined duration - 1 day storm intensity
#### no plot generated
#compute_storm_index_for_user_defined_regions(sourceDir = "input", 
#                                             destDir = "output",
#                                             user.region.name = "Larger_Sydney",
#                                             duration = "1-day")


#### Generate storm index for user defined region, 
#### based on user defined duration - 5 day storm intensity
#### no plot generated
#compute_storm_index_for_user_defined_regions(sourceDir = "input", 
#                                             destDir = "output",
#                                             user.region.name = "Larger_Sydney",
#                                             duration = "5-day")

#### Generate storm return time for user defined region, 
#### based on user defined duration - 1 & 5 day storm intensity
#### no plot generated
compute_storm_return_time_for_user_defined_regions(sourceDir = "input", 
                                             destDir = "output",
                                             user.region.name = "Larger_Sydney",
                                             duration = 1)
compute_storm_return_time_for_user_defined_regions(sourceDir = "input", 
                                             destDir = "output",
                                             user.region.name = "Larger_Sydney",
                                             duration = 5)

#### 2. Calculate drought index:
####     Drought index has duration options of consecutive no rain days, 1-year, 2-year,
####     Output a 3 dimension matrix with lat lon and 9 layers of storm index
####     Each layer is the number of no rain days, 
####     0.1th, 1th, 5th, 10th, 20th, 30th, 40th, 50th percentile of the rainfall distribution

#### placeholder for computing drought index for whole Australia (merging 23 rds)
#merge_and_compute_Australia_drought_index(sourceDir = "/Volumes/TOSHIBAEXT/AWAP/output",
#                                          destDir = "/Volumes/TOSHIBAEXT/AWAP/output",
#                                          duration = "1-year")

#### Generate drought index for user defined region, 
#### based on user defined duration - antecedent 1-year rainfall total
#### no plot generated
#compute_drought_index_for_user_defined_regions(sourceDir = "input", 
#                                               destDir = "output",
#                                               user.region.name = "Larger_Sydney",
#                                               duration = "1-year")


#### Generate drought index for user defined region, 
#### based on user defined duration - antecedent 2-year rainfall total
#### no plot generated
#compute_drought_index_for_user_defined_regions(sourceDir = "input", 
#                                               destDir = "output",
#                                               user.region.name = "Larger_Sydney",
#                                               duration = "2-year")


#### 3. Process wind data to calculate wind index
####    Just one-day max wind and gust speed.
####    This is GSOD station based dataset,
####    so we have data gaps and spatial representative issues
#process_GSOD_station_data(sourceDir = "/Volumes/TOSHIBAEXT/gsod/", 
#                          destDir = "output",
#                          user.region.name = "Larger_Sydney",
#                          user.lat.max = -28,
#                          user.lat.min = -36,
#                          user.lon.max = 155,
#                          user.lon.min = 145,
#                          plot.option = T)

#### After data processing, plot wind speed spatial map
plot_GSOD_station_wind_data_for_user_selected_regions(sourceDir = "output",
                                                      destDir = "output/plots",
                                                      user.region.name = "Larger_Sydney",
                                                      user.lat.max = -28,
                                                      user.lat.min = -36,
                                                      user.lon.max = 155,
                                                      user.lon.min = 145)


### +++++++++++++++ End basic code to generate climate extreme index ++++++++++++++++++ ####
############################################################################################


###########################################################################################
#### +++++++++++++++ Climate extreme severity investigation ++++++++++++++++++++++++++ ####
#### Structure: 
#### 1. compute drought and storm severity for a selected region and date
#### 2. compute wind severity for a selected region and date
#### 3 and so on: repeat 1 and 2 for different region/date

#### 1. For each extreme rainfall event, obtain the drought severity information
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
compute_drought_and_storm_event_severity_for_user_defined_regions(sourceDir = "input", 
                                                                  destDir = "output",
                                                                  user.region.name = "Larger_Sydney",
                                                                  date.of.interest = "20191126",
                                                                  storm.duration = "1-day",
                                                                  drought.duration = "1-year")


compute_wind_event_severity_for_user_defined_regions(sourceDir = "/Volumes/TOSHIBAEXT/gsod/",
                                                     destDir = "output",
                                                     user.region.name = "Larger_Sydney",
                                                     date.of.interest = "20191126")


#### Repeat for different duration/region option
compute_drought_and_storm_event_severity_for_user_defined_regions(sourceDir = "input", 
                                                                  destDir = "output",
                                                                  user.region.name = "Larger_Sydney",
                                                                  date.of.interest = "20191126",
                                                                  storm.duration = "5-day",
                                                                  drought.duration = "1-year")


#### Repeat for different duration/region option
compute_drought_and_storm_event_severity_for_user_defined_regions(sourceDir = "input", 
                                                                  destDir = "output",
                                                                  user.region.name = "Larger_Sydney",
                                                                  date.of.interest = "20191126",
                                                                  storm.duration = "5-day",
                                                                  drought.duration = "2-year")


### +++++++++++++++ Climate extreme severity investigation +++++++++++++++++++++++++++ ####
############################################################################################


############################################################################################
### ++++++++++++++++++++++++++++++++++ Start plotting +++++++++++++++++++++++++++++++++ ####
#### Structure: 
#### 1. Plot Australia extreme
#### 2. Plot selected region severity and intensity maps

##### 1.1. plot Australia storm extreme
plot_Australia_storm_extreme_DF(sourceDir = "/Volumes/TOSHIBAEXT/AWAP/output",
                                destDir = "output",
                                duration = "1-day",
                                plot.option = T)


##### 1.2. plot Australia storm extreme
plot_Australia_drought_extreme_DF(sourceDir = "/Volumes/TOSHIBAEXT/AWAP/output",
                                destDir = "output",
                                duration = "1-year",
                                plot.option = T)

#### 2. Plot selected region severity and intensity maps
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
make_spatial_plots_for_user_defined_regions(sourceDir = "output",
                                            destDir = "output/plots",
                                            user.region.name = "Larger_Sydney",
                                            date.of.interest = "20191126",
                                            user.lat.max = -28,
                                            user.lat.min = -36,
                                            user.lon.max = 155,
                                            user.lon.min = 145,
                                            storm.duration = "1-day",
                                            drought.duration = "1-year")



make_spatial_plots_for_user_defined_regions(sourceDir = "output",
                                            destDir = "output/plots",
                                            user.region.name = "Larger_Sydney",
                                            date.of.interest = "20191126",
                                            user.lat.max = -28,
                                            user.lat.min = -36,
                                            user.lon.max = 155,
                                            user.lon.min = 145,
                                            storm.duration = "5-day",
                                            drought.duration = "1-year")


make_spatial_plots_for_user_defined_regions(sourceDir = "output",
                                            destDir = "output/plots",
                                            user.region.name = "Larger_Sydney",
                                            date.of.interest = "20191126",
                                            user.lat.max = -28,
                                            user.lat.min = -36,
                                            user.lon.max = 155,
                                            user.lon.min = 145,
                                            storm.duration = "5-day",
                                            drought.duration = "2-year")


#### Sydney and Hunter valley region only
sourceDir = "output"
destDir = "output/plots"
user.region.name = "Larger_Sydney"
date.of.interest = "20191126"
user.lat.max = -28
user.lat.min = -36
user.lon.max = 155
user.lon.min = 145
storm.duration = "1-day"
drought.duration = "1-year"
make_spatial_plots_for_user_defined_regions(sourceDir = "output",
                                            destDir = "output/plots",
                                            user.region.name = "Larger_Sydney",
                                            date.of.interest = "20191126",
                                            user.lat.max = -28,
                                            user.lat.min = -36,
                                            user.lon.max = 155,
                                            user.lon.min = 145,
                                            storm.duration = "1-day",
                                            drought.duration = "1-year")


### ++++++++++++++++++++++++++++++++++++ End plotting +++++++++++++++++++++++++++++++++ ####
############################################################################################

### to do list:
### 3. Revise code to speed up the selected region extreme severity checking script
### 4. Clean code to make it more reader-friendly
### 5. Create spider chart to combine indices on wind, storm and drought 
### 6. Prepare codes to generate results on all needed dates 
### 7. Write result interpretation


