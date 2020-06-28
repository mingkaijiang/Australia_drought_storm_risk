############### Risk Assessment of Drought and Storm Risk to Power Grid ###############
################################## Master sript #######################################
### +++++++ code developed by: Mingkai Jiang (m.jiang@westernsydney.edu.au) +++++++ ###
#######################################################################################



#######################################################################################
### +++++++++++++++++++++++++++++ General set-up ++++++++++++++++++++++++++++++++++ ###
### clear wk space
rm(list=ls(all=TRUE))

### source all necessary files
source("prepare.R")

###+++++++++++++++++++++++++++++++ End general set-up ++++++++++++++++++++++++++++++++####
##########################################################################################





###########################################################################################
### +++++++++++++++++++++ Basic code to process the raw data +++++++++++++++++++++++++ ####
#### Structure:
#### 1. Download data
#### 2. Unzip files
#### 3. Data quality check: Plot year 2019 and check with observation
#### 4. Convert data from per day to per grid, for whole Australia
#### 5. Convert data from per day to per grid, for user defined regions
#### 6. Calculate VPD based on: 
#### 6.1. Calculate saturated vapor pressure based on Tmax
#### 6.2. Calculate VPD based on ES and EA (VP at 3 pm)
#### 7. Calculate PET based on Tmax

#### 1 . Download AWAP data from BOM website
####     Note: Only need to run this section of code once!!!
### 1.1. Daily rainfall data - from 1900 to 2020 (march 31st)
#download_AWAP_rainfall_data(destDir="/Volumes/TOSHIBAEXT/AWAP/rain/")

### 1.2. Daily maximum temperature - from 1911 to 2020 (march 31st)
#download_AWAP_temperature_data(destDir="/Volumes/TOSHIBAEXT/AWAP/tmax/")

### 1.3. Vapor pressure at 3 pm - from 1971 to 2020 (march 31st)
#download_AWAP_vp3pm_data(destDir="/Volumes/TOSHIBAEXT/AWAP/vp3pm/")



#### 2. Unzip all .z files
####    Only need to run this section of code once
#unzip_all_z_files(sourceDir = "/Volumes/TOSHIBAEXT/AWAP/rain/", s.yr = 1900, e.yr = 2020)

#unzip_all_z_files(sourceDir = "/Volumes/TOSHIBAEXT/AWAP/tmax/", s.yr = 1911, e.yr = 2020)

#unzip_all_z_files(sourceDir = "/Volumes/TOSHIBAEXT/AWAP/vp3pm/", s.yr = 1971, e.yr = 2020)


#### 3. Data quality check: Plot one-year total rainfall to check rmatches with BOM observations
#plot_total_rainfall_for_a_year(sourceDir = "/Volumes/TOSHIBAEXT/AWAP/rain/", 
#                               destDir = "plots",
#                               user.defined.year = 2019)

#plot_daily_tmax_for_a_year(sourceDir = "/Volumes/TOSHIBAEXT/AWAP/tmax/", 
#                           destDir = "plots",
#                           user.defined.year = 2019)

#plot_daily_vp3pm_for_a_year(sourceDir = "/Volumes/TOSHIBAEXT/AWAP/vp3pm/", 
#                           destDir = "plots",
#                           user.defined.year = 2019)


#### 4. Convert from per day to per grid
#### Only need to run this code once
#### For whole Australia
#### only need to run once, takes long to run (2 days)
#convert_from_spatial_to_temporal_DF_whole_Australia(sourceDir = "/Volumes/TOSHIBAEXT/AWAP/rain/", 
#                                                    destDir = "/Volumes/TOSHIBAEXT/AWAP/output")


#### 5. make user specified selection of spatial range
#### Note that, region has to be small (i.e. ~ 10 by 10 degree) to not exceed memory limit
#### User also need to specify region name.
#### Only need to run once, takes long to run (2 hour for 10 by 10)

#### 5.1 daily rainfall
#convert_from_spatial_to_temporal_DF_for_user_defined_regions_rain(sourceDir = "/Volumes/TOSHIBAEXT/AWAP/rain/", 
#                                                                  destDir = "input",
#                                                                  varName = "rain",
#                                                                  user.lat.max = -31,
#                                                                  user.lat.min = -35,
#                                                                  user.lon.max = 153,
#                                                                  user.lon.min = 149,
#                                                                  user.region.name = "SydneyHunter")

#### 5.2. daily Tmax
#convert_from_spatial_to_temporal_DF_for_user_defined_regions_tmax(sourceDir = "/Volumes/TOSHIBAEXT/AWAP/tmax/", 
#                                                                  destDir = "input",
#                                                                  varName = "tmax",
#                                                                  user.lat.max = -31,
#                                                                  user.lat.min = -35,
#                                                                  user.lon.max = 153,
#                                                                  user.lon.min = 149,
#                                                                  user.region.name = "SydneyHunter")

### 5.3 daily vapor pressure at 3 pm
#convert_from_spatial_to_temporal_DF_for_user_defined_regions_vp3pm(sourceDir = "/Volumes/TOSHIBAEXT/AWAP/vp3pm/", 
#                                                                   destDir = "input",
#                                                                   varName = "vp3pm",
#                                                                   user.lat.max = -31,
#                                                                   user.lat.min = -35,
#                                                                   user.lon.max = 153,
#                                                                   user.lon.min = 149,
#                                                                   user.region.name = "SydneyHunter")


#### 6. Calculate VPD based on Tmax and vp3pm
### 6.1. Calculate saturated vapor pressure based on Tmax
#calculate_saturated_vapor_pressure_based_on_Tmax(sourceDir = "input",
#                                                 destDir = "input",
#                                                 varName = "es",
#                                                 user.region.name = "SydneyHunter")

### 6.2. Calculate VPD based on ES and EA
####     Note: check back - VPD range - 30 to + 6, possibly not right !!!!
#calculate_VPD_based_on_es_and_vp3pm(sourceDir = "input",
#                                    destDir = "input",
#                                    varName = "vpd",
#                                    user.region.name = "SydneyHunter")
#
#
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

#### Note: the station-based wind speed data is processed later in the next section

### +++++++++++++++++++++ End basic code to process the raw data +++++++++++++++++++++ ####
###########################################################################################





###########################################################################################
### ++++++++++++++++++++++ Generate climate extreme index ++++++++++++++++++++++++++++ ####
#### Structure:
#### A. Australia
#### A1. Storm index (1-day rainfall intensity) for whole Australia (merging 23 RDS datasets)
#### A2. Drought index (antecedent 1-year rainfall total) for whole Australia (merging 23 RDS datasets)

#### B. User selected region:
#### B1. Calculate user selected region gridded short-term storm index (1 and 5 day rainfall intensity)
#### B2. Calculate user selected region gridded short-term storm return index (1 and 5 day rainfall intensity return interval)
#### B3. Calculate user selected region station-based wind index (daily max wind speed)
#### B4. Calculate user selected region antecedent water availability index (1 and 2 year antecedent rainfall total)
#### B5. Calculate user selected region antecedent atmospheric index (mean VPD for 1 and 2 year antecendent period)
#### B6. Calculate user selected region antecedent water deficit index (PET - P for 1 and 2 year antecedent period)

#### A. Australia
#### A1. placeholder for computing storm index for whole Australia (merging 23 rds data)
#merge_and_compute_Australia_storm_index(sourceDir = "/Volumes/TOSHIBAEXT/AWAP/output",
#                                        destDir = "/Volumes/TOSHIBAEXT/AWAP/output",
#                                        duration = "1-day")


#### A2. placeholder for computing drought index for whole Australia (merging 23 rds)
#merge_and_compute_Australia_drought_index(sourceDir = "/Volumes/TOSHIBAEXT/AWAP/output",
#                                          destDir = "/Volumes/TOSHIBAEXT/AWAP/output",
#                                          duration = "1-year")
################################################################



#### B. User selected region (i.e. Larger sydney regions)
#### B1. Calculate storm index, based on Sydney region daily data;
####     Storm index has duration options of 1 - 5 days
####     Output a 3 dimension matrix with lat lon and 9 layers of storm index
####     Each layer is the 99.9th, 99th, 95th, 90th, 80th, 70th, 60th, 50th, 40th percentile
####     Generate storm index for user defined region, 
####     based on user defined duration - 1 day storm intensity.
####     No plot generated.

#### B1.1. 1-day storm intensity
#compute_storm_index_for_user_defined_regions(sourceDir = "input", 
#                                             destDir = "output/storm",
#                                             user.region.name = "SydneyHunter",
#                                             duration = "1-day")


#### B1.2. 5-day storm intensity
#compute_storm_index_for_user_defined_regions(sourceDir = "input", 
#                                             destDir = "output/storm",
#                                             user.region.name = "SydneyHunter",
#                                             duration = "5-day")


#### B2. Generate storm return time for user defined region, 
####     based on user defined duration - 1 & 5 day storm intensity
#### B2.1. 1-day rainfall intensity
#compute_storm_return_time_for_user_defined_regions(sourceDir = "input", 
#                                                   destDir = "output/storm",
#                                                   user.region.name = "SydneyHunter",
#                                                   duration = 1)

#### B2.2. 5-day rainfall intensity
#compute_storm_return_time_for_user_defined_regions(sourceDir = "input", 
#                                                   destDir = "output/storm",
#                                                   user.region.name = "SydneyHunter",
#                                                   duration = 5)


#### B3. Process wind data to calculate wind index

#### B3.1. One-day max wind speed extracted from the GSOD dataset. 
####      We have data gaps and spatial representative issues in the dataset,
####      but they do not matter for our purpose here.
####      After data processing, plot wind speed spatial map for whole Australia
####      We also calculated wind speed intensity and return interval percentile within this function.
#process_GSOD_station_data(sourceDir = "/Volumes/TOSHIBAEXT/gsod/", 
#                          destDir = "input",
#                          user.lat.max = -31,
#                          user.lat.min = -35,
#                          user.lon.max = 153,
#                          user.lon.min = 149,
#                          user.region.name = "SydneyHunter",
#                          plot.option = T)
#
##### B3.2. Plot wind speed percentile for user selected region
#plot_GSOD_station_wind_data_for_user_selected_regions(sourceDir = "input",
#                                                      destDir = "plots",
#                                                      user.lat.max = -31,
#                                                      user.lat.min = -35,
#                                                      user.lon.max = 153,
#                                                      user.lon.min = 149,
#                                                      user.region.name = "SydneyHunter")



#### B4. Calculate antecedent 1 and 2 year rainfall availability:
####     Drought index has duration options of consecutive no rain days, 1-year, 2-year,
####     Output a 3 dimension matrix with lat lon and 9 layers of storm index
####     Each layer is the number of no rain days, 
####     0.1th, 1th, 5th, 10th, 20th, 30th, 40th, 50th percentile of the rainfall distribution.
####     Generate drought index for user defined region, 
####     based on user defined duration - antecedent 1-year rainfall total.
####     No plot generated

#### B4.1. antecedent 1-year water availability
#compute_antecedent_water_availability_for_user_defined_regions(sourceDir = "input", 
#                                                               destDir = "output/antecedent_water_availability",
#                                                               user.region.name = "SydneyHunter",
#                                                               duration = "1-year")


#### B4.2. antecedent 2-year water availability
#compute_antecedent_water_availability_for_user_defined_regions(sourceDir = "input", 
#                                                               destDir = "output/antecedent_water_availability",
#                                                               user.region.name = "SydneyHunter",
#                                                               duration = "2-year")


#### B5. calculate atmospheric dryness (VPD) for antecedent 1 and 2-year period
#### B5.1. atmospheric dryness for antecendent 1-year period
compute_antecedent_atmospheric_dryness_for_user_defined_regions(sourceDir = "input", 
                                                               destDir = "output/antecedent_atmospheric_dryness",
                                                               user.region.name = "SydneyHunter",
                                                               duration = "1-year")
### B5.2. atmospheric dryness for antecedent 2-year period
compute_antecedent_atmospheric_dryness_for_user_defined_regions(sourceDir = "input", 
                                                                destDir = "output/antecedent_atmospheric_dryness",
                                                                user.region.name = "SydneyHunter",
                                                                duration = "2-year")



#### B6. calculate water deficit (PET - P) drought intex for antecedent 1 and 2 year period

#### B6.1. water deficit for 1-year period
#compute_antecedent_water_deficit_for_user_defined_regions(sourceDir = "input", 
#                                                          destDir = "output/antecedent_water_deficit",
#                                                          user.region.name = "SydneyHunter",
#                                                          duration = "1-year")
#
##### B6.2. water deficit for 2-year period
#compute_antecedent_water_deficit_for_user_defined_regions(sourceDir = "input", 
#                                                          destDir = "output/antecedent_water_deficit",
#                                                          user.region.name = "SydneyHunter",
#                                                          duration = "2-year")


### ++++++++++++++++++++++ End generate climate extreme index +++++++++++++++++++++++++ ####
############################################################################################






###########################################################################################
#### ++++++++++ Investigate climate extreme severity for date of interest ++++++++++++ ####
####    For each user specified date, obtain the historic extreme percentile information
####    User can specify a particular date.of.interest,
####    the script will calculate the short-term rainfall storm intensity, wind speed, 
####    long-term antecedent water availability, antecedent atmospheric dryness, antecedent
####    water deficit, based on user-defined duration.
####    The script will then compare against the all-time percentile,
####    to indicate the severity of the current event on the given date.
####    User-defined storm.duration can be: 1 or 5 days
####    User-defined drought.duration can be: 1 or 2 years
####    The script will run over the selected region.

#### Structure: 
#### A. date of interest: 20191126
####    region affected: Hornsby Shire, Ku-ring-gai, Lane Cove, 
####                     Northern Beaches, Sutherland Shire, Willoughby
#### A1. check storm intensity severity for date of selection over the user defined region
#### A1.1. storm intensity over 1-day period
#### A1.2. storm intensity over 5-day period

#### A2. check wind intensity severity for date of selection over the user defined region

#### A3. check antecedent water availability for date of selection over the user defined region
#### A3.1. antecedent 1-year water availability
#### A3.2. antecedent 2-year water availability

#### A4. check antecedent atmospheric dryness (VPD) for date of selection over the user defined region
#### A4.1. antecedent 1-year atmospheric dryness
#### A4.2. antecedent 2-year atmospheric dryness

#### A5. check antecedent water deficit (PET - P) for date of selection over the user defined region
#### A5.1. antecedent 1-year water deficit
#### A5.2. antecedent 2-year water deficit

#### B. date of interest: 20200208
####    region affected: Burwood, Canada Bay, Central Coast, Cessnock, Georges River, 
####                     Hornsby, Hunters Hill, Inner West, Ku-ring-gai, Lake Macquarie, 
####                     Lane Cove, Maitland, Mosman, Newcastle, North Sydney, Northern Beaches,
####                     Port Stephens, Randwick, Ryde, Stathfield, Sutherland, Sydney City, 
####                     Waverly, Willoughby, Woollahra

#### C. date of interest: 20200218
####    region affected: same as B (i.e. Burwood, Canada Bay, Central Coast, Cessnock, Georges River, 
####                     Hornsby, Hunters Hill, Inner West, Ku-ring-gai, Lake Macquarie, 
####                     Lane Cove, Maitland, Mosman, Newcastle, North Sydney, Northern Beaches,
####                     Port Stephens, Randwick, Ryde, Stathfield, Sutherland, Sydney City, 
####                     Waverly, Willoughby, Woollahra)
########################################################

#### A. date of interest: 20191126
####    region affected: Hornsby Shire, Ku-ring-gai, Lane Cove, 
####                     Northern Beaches, Sutherland Shire, Willoughby
#### A1. check storm intensity severity for date of selection over the user defined region
#### A1.1. storm intensity over 1-day period, 
#### and storm intensity expressed by return intervals
#compute_storm_severity_for_user_defined_regions(sourceDir = "input", 
#                                                destDir = "output/storm",
#                                                user.region.name = "SydneyHunter",
#                                                date.of.interest = "20191126",
#                                                storm.duration = "1-day")
#
##### A1.2. storm intensity over 5-day period
##### and storm intensity expressed by return intervals
#compute_storm_severity_for_user_defined_regions(sourceDir = "input", 
#                                                destDir = "output/storm",
#                                                user.region.name = "SydneyHunter",
#                                                date.of.interest = "20191126",
#                                                storm.duration = "5-day")
#
#
##### B1.1 and 1.2
#compute_storm_severity_for_user_defined_regions(sourceDir = "input", 
#                                                destDir = "output/storm",
#                                                user.region.name = "SydneyHunter",
#                                                date.of.interest = "20200208",
#                                                storm.duration = "1-day")
#
#compute_storm_severity_for_user_defined_regions(sourceDir = "input", 
#                                                destDir = "output/storm",
#                                                user.region.name = "SydneyHunter",
#                                                date.of.interest = "20200208",
#                                                storm.duration = "5-day")
#
#
##### C1.1 and 1.2
#compute_storm_severity_for_user_defined_regions(sourceDir = "input", 
#                                                destDir = "output/storm",
#                                                user.region.name = "SydneyHunter",
#                                                date.of.interest = "20200218",
#                                                storm.duration = "1-day")
#
#compute_storm_severity_for_user_defined_regions(sourceDir = "input", 
#                                                destDir = "output/storm",
#                                                user.region.name = "SydneyHunter",
#                                                date.of.interest = "20200218",
#                                                storm.duration = "5-day")
#
#
#
#
#
##### A2. check wind intensity severity for date of selection over the user defined region
#compute_wind_event_severity_for_user_defined_regions(sourceDir1 = "/Volumes/TOSHIBAEXT/gsod/",
#                                                     sourceDir2 = "input",
#                                                     destDir = "output/wind",
#                                                     user.region.name = "SydneyHunter",
#                                                     date.of.interest = "20191126")
#
##### B2. 
#compute_wind_event_severity_for_user_defined_regions(sourceDir1 = "/Volumes/TOSHIBAEXT/gsod/",
#                                                     sourceDir2 = "input",
#                                                     destDir = "output/wind",
#                                                     user.region.name = "SydneyHunter",
#                                                     date.of.interest = "20200208")
#
##### C2. 
#compute_wind_event_severity_for_user_defined_regions(sourceDir1 = "/Volumes/TOSHIBAEXT/gsod/",
#                                                     sourceDir2 = "input",
#                                                     destDir = "output/wind",
#                                                     user.region.name = "SydneyHunter",
#                                                     date.of.interest = "20200218")
#
#
#
#
##### A3. check antecedent water availability for date of selection over the user defined region
##### A3.1. antecedent 1-year water availability
#compute_antecedent_water_availability_severity_for_user_defined_regions(sourceDir = "input", 
#                                                                        destDir = "output/antecedent_water_availability",
#                                                                        user.region.name = "SydneyHunter",
#                                                                        date.of.interest = "20191126",
#                                                                        drought.duration = "1-year")
#
##### A3.2. antecedent 2-year water availability
#compute_antecedent_water_availability_severity_for_user_defined_regions(sourceDir = "input", 
#                                                                        destDir = "output/antecedent_water_availability",
#                                                                        user.region.name = "SydneyHunter",
#                                                                        date.of.interest = "20191126",
#                                                                        drought.duration = "2-year")
#
##### B3
#compute_antecedent_water_availability_severity_for_user_defined_regions(sourceDir = "input", 
#                                                                        destDir = "output/antecedent_water_availability",
#                                                                        user.region.name = "SydneyHunter",
#                                                                        date.of.interest = "20200208",
#                                                                        drought.duration = "1-year")
#
#compute_antecedent_water_availability_severity_for_user_defined_regions(sourceDir = "input", 
#                                                                        destDir = "output/antecedent_water_availability",
#                                                                        user.region.name = "SydneyHunter",
#                                                                        date.of.interest = "20200208",
#                                                                        drought.duration = "2-year")
#
##### C3
#compute_antecedent_water_availability_severity_for_user_defined_regions(sourceDir = "input", 
#                                                                        destDir = "output/antecedent_water_availability",
#                                                                        user.region.name = "SydneyHunter",
#                                                                        date.of.interest = "20200218",
#                                                                        drought.duration = "1-year")
#
#compute_antecedent_water_availability_severity_for_user_defined_regions(sourceDir = "input", 
#                                                                        destDir = "output/antecedent_water_availability",
#                                                                        user.region.name = "SydneyHunter",
#                                                                        date.of.interest = "20200218",
#                                                                        drought.duration = "2-year")
#
#
#
#### A4. check antecedent atmospheric dryness (VPD) for date of selection over the user defined region
#### A4.1. antecedent 1-year atmospheric dryness
compute_antecedent_atmospheric_dryness_severity_for_user_defined_regions(sourceDir = "input", 
                                                                         destDir = "output/antecedent_atmospheric_dryness",
                                                                         user.region.name = "SydneyHunter",
                                                                         date.of.interest = "20191126",
                                                                         duration = "1-year")

#### A4.2. antecedent 2-year atmospheric dryness
compute_antecedent_atmospheric_dryness_severity_for_user_defined_regions(sourceDir = "input", 
                                                                         destDir = "output/antecedent_atmospheric_dryness",
                                                                         user.region.name = "SydneyHunter",
                                                                         date.of.interest = "20191126",
                                                                         duration = "2-year")

#### B4
compute_antecedent_atmospheric_dryness_severity_for_user_defined_regions(sourceDir = "input", 
                                                                         destDir = "output/antecedent_atmospheric_dryness",
                                                                         user.region.name = "SydneyHunter",
                                                                         date.of.interest = "20200208",
                                                                         duration = "1-year")

compute_antecedent_atmospheric_dryness_severity_for_user_defined_regions(sourceDir = "input", 
                                                                         destDir = "output/antecedent_atmospheric_dryness",
                                                                         user.region.name = "SydneyHunter",
                                                                         date.of.interest = "20200208",
                                                                         duration = "2-year")


#### C4
compute_antecedent_atmospheric_dryness_severity_for_user_defined_regions(sourceDir = "input", 
                                                                         destDir = "output/antecedent_atmospheric_dryness",
                                                                         user.region.name = "SydneyHunter",
                                                                         date.of.interest = "20200218",
                                                                         duration = "1-year")

compute_antecedent_atmospheric_dryness_severity_for_user_defined_regions(sourceDir = "input", 
                                                                         destDir = "output/antecedent_atmospheric_dryness",
                                                                         user.region.name = "SydneyHunter",
                                                                         date.of.interest = "20200218",
                                                                         duration = "2-year")




#### A5. check antecedent water deficit (PET - P) for date of selection over the user defined region
#### A5.1. antecedent 1-year water deficit
#compute_antecedent_water_deficit_severity_for_user_defined_regions(sourceDir = "input", 
#                                                                   destDir = "output/antecedent_water_deficit",
#                                                                   user.region.name = "SydneyHunter",
#                                                                   user.lat.max = -31,
#                                                                   user.lat.min = -35,
#                                                                   user.lon.max = 153,
#                                                                   user.lon.min = 149,
#                                                                   date.of.interest = "20191126",
#                                                                   duration = "1-year")
#
##### A5.2. antecedent 2-year water deficit
#compute_antecedent_water_deficit_severity_for_user_defined_regions(sourceDir = "input", 
#                                                                   destDir = "output/antecedent_water_deficit",
#                                                                   user.region.name = "SydneyHunter",
#                                                                   user.lat.max = -31,
#                                                                   user.lat.min = -35,
#                                                                   user.lon.max = 153,
#                                                                   user.lon.min = 149,
#                                                                   date.of.interest = "20191126",
#                                                                   duration = "2-year")
#
##### B5.
#compute_antecedent_water_deficit_severity_for_user_defined_regions(sourceDir = "input", 
#                                                                   destDir = "output/antecedent_water_deficit",
#                                                                   user.region.name = "SydneyHunter",
#                                                                   user.lat.max = -31,
#                                                                   user.lat.min = -35,
#                                                                   user.lon.max = 153,
#                                                                   user.lon.min = 149,
#                                                                   date.of.interest = "20200208",
#                                                                   duration = "1-year")
#
#compute_antecedent_water_deficit_severity_for_user_defined_regions(sourceDir = "input", 
#                                                                   destDir = "output/antecedent_water_deficit",
#                                                                   user.region.name = "SydneyHunter",
#                                                                   user.lat.max = -31,
#                                                                   user.lat.min = -35,
#                                                                   user.lon.max = 153,
#                                                                   user.lon.min = 149,
#                                                                   date.of.interest = "20200208",
#                                                                   duration = "2-year")
#
#
##### C5. 
#compute_antecedent_water_deficit_severity_for_user_defined_regions(sourceDir = "input", 
#                                                                   destDir = "output/antecedent_water_deficit",
#                                                                   user.region.name = "SydneyHunter",
#                                                                   user.lat.max = -31,
#                                                                   user.lat.min = -35,
#                                                                   user.lon.max = 153,
#                                                                   user.lon.min = 149,
#                                                                   date.of.interest = "20200218",
#                                                                   duration = "1-year")
#
#compute_antecedent_water_deficit_severity_for_user_defined_regions(sourceDir = "input", 
#                                                                   destDir = "output/antecedent_water_deficit",
#                                                                   user.region.name = "SydneyHunter",
#                                                                   user.lat.max = -31,
#                                                                   user.lat.min = -35,
#                                                                   user.lon.max = 153,
#                                                                   user.lon.min = 149,
#                                                                   date.of.interest = "20200218",
#                                                                   duration = "2-year")
#


#### ++++++++++ End investigate climate extreme severity for date of interest +++++++++ ####
############################################################################################






############################################################################################
### ++++++++++++++++++++++++++++++++++++ Plotting +++++++++++++++++++++++++++++++++++++ ####
#### Structure: 
#### A. Plot Australia extreme
#### B. Plot selected region severity and intensity maps
####    Each figure includes: two panel plots (left percentile, right actual value) for each index;
####    Three dates: 20191126, 20200208, 20200218, only for Sydney zoomed-in region
####    Index includes: drought intensity: 1). 1-year and 2). 2-year antecedent rainfall
####                    storm intensity return interval: 3). 1-day and 4). 5-day rainfall total
####                    wind speed: 5). 1-day max wind speed, or wind speed return interval
####                    VPD intensity: 6). 1-year and 7). 2-year antecedent atmospheric dryness
####                    PD intensity: 8). 1-year and 9). 2-year antecedent precipitation deficit (drought + temperature, i.e. P - PET)
### In each case, we should map actual values, alongside normalised frequency map


##### A.1. plot Australia storm extreme
#plot_Australia_storm_extreme_DF(sourceDir = "/Volumes/TOSHIBAEXT/AWAP/output",
#                                destDir = "output",
#                                duration = "1-day",
#                                plot.option = T)
#
#
###### A.2. plot Australia storm extreme
#plot_Australia_drought_extreme_DF(sourceDir = "/Volumes/TOSHIBAEXT/AWAP/output",
#                                destDir = "output",
#                                duration = "1-year",
#                                plot.option = T)

#### B. Plot selected region (i.e. Sydney and Hunter Valley) extreme severity and intensity maps

#### B.1. 20191126
sourceDir = "output"
destDir = "plots"
user.region.name = "SydneyHunter"
date.of.interest = "20191126"
make_spatial_plots_for_Sydney_Hunter_valley_regions(sourceDir = "output",
                                                    destDir = "plots",
                                                    user.region.name = "SydneyHunter",
                                                    date.of.interest = "20191126")

#### B.2. 20200208
sourceDir = "output"
destDir = "plots"
user.region.name = "SydneyHunter"
date.of.interest = "20200208"
make_spatial_plots_for_Sydney_Hunter_valley_regions(sourceDir = "output",
                                                    destDir = "plots",
                                                    user.region.name = "SydneyHunter",
                                                    date.of.interest = "20200208")


#### B.3. 20200218
sourceDir = "output"
destDir = "plots"
user.region.name = "SydneyHunter"
date.of.interest = "20200218"
make_spatial_plots_for_Sydney_Hunter_valley_regions(sourceDir = "output",
                                                    destDir = "plots",
                                                    user.region.name = "SydneyHunter",
                                                    date.of.interest = "20200218")




### ++++++++++++++++++++++++++++++++++++ End plotting +++++++++++++++++++++++++++++++++ ####
############################################################################################


#######################
### to do list:
### 1. VPD bug fixed (mean, not sum), checking results and unit and spatial pattern. 
### 2. Check P - PET spatial pattern
### 3. Update VPD unit (currently all positive, convert to negative)
### 4. Improve plot:
###          a) add all variables to plotting scheme
###          b) make nicer plots (add DEM? add cities and borders, forest cover?)


