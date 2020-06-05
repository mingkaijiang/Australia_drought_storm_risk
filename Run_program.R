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
#### 1. prepare storage DF
#### 2. loop through custom-defined year, 
####    and perform monthly mean, sd and n calculation, based on 6-hourly data
#### 3. prepare annual storage DF
#### 4. prepare sea surface mask



### +++++++++++++++ End basic code to generate climate extreme index ++++++++++++++++++ ####
############################################################################################

