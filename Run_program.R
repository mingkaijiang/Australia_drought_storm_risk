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
#### 1. Process raw data 
#### 2. Calculate climate extremes with alternative ways
####    a. drought index - duration (count number of days with no or little rainfall)
####    b. storm index - rainfall intensity over one / three / five / ten days after drought

#### 1. Unzip all .z files
####    Only need to run this code once
#unzip_all_z_files(s.yr = 1900, e.yr = 2018)


#### 2. Calculate drought index:
#### For each grid, merge all daily data,
#### Then count number of days with no rainfall (or very limited rainfall),
#### Then compute distribution to get the extreme drought threshold.
#### When counting number of drought days, output start and end date.
compute_drought_index(sourceDir = "/Volumes/Volumes/TOSHIBAEXT/AWAP/rain/", 
                      destDir = "output")


#### 3.1. Calculate storm index, based on daily data;
#### For each grid, merge all daily data,
#### Then compute distribution of rainfall
#### Then get the extreme rainfall threshold
#### Also output start and end date for the extreme rainfall
compute_storm_index_daily()


#### 3.2. Same as 3.1, but based on 3-day total rainfall


#### 3.3. Same as 3.1, but based on 5 day total rainfall


#### 3.4. Same as 3.1, but based on 7 day total rainfall


#### 4. For each extreme rainfall event, obtain number of droughted days before the rainfall event
compute_drought_before_storm_event()


#### 5. Investigate how extreme the drought is before the storm event
check_drought_extremeness_before_storm_event()


#### 6. Make spatial plots



#### 7. Make statistics

### +++++++++++++++ End basic code to generate climate extreme index ++++++++++++++++++ ####
############################################################################################

