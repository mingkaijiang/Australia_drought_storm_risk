compute_wind_event_severity_for_user_defined_regions <- function(sourceDir,
                                                                 destDir,
                                                                 user.region.name,
                                                                 date.of.interest) {
    
    ### year of interest
    year.of.interest <- substr(date.of.interest, 1, 4)
    
    ### read in extreme index
    gsodDF <- read.csv(paste0("output/GSOD_Wind_Extreme_", user.region.name,
                              "_regions.csv"))
    
    n.station <- dim(gsodDF)[1]
    
    gsodDF$station.id <- paste0(gsodDF$USAF, "-", gsodDF$WBAN)
    
    gsodDF$file <- paste0(gsodDF$station.id, "-", year.of.interest, ".op.gz")
    
    gsodDF$path <- paste0(sourceDir, "gsod_", year.of.interest, "/")

    gsodDF$filepath <- paste0(sourceDir, "gsod_", year.of.interest, "/",
                              gsodDF$station.id, "-", year.of.interest, ".op.gz")
    
    ### pre-define variable
    gsodDF$selected.wind.speed <- NA
    
    ### read in GSOD station data
    ### loop through each station
    for (i in c(1:n.station)) {
        
        all.file.names <- list.files(path = gsodDF$path[i], pattern = "*.op.gz")
        
        ## check of the file exists
        if(gsodDF$file[i] %in% all.file.names) {
            
            ## print file found
            print("file found")
            
            ## read in data
            gz <- gzfile(gsodDF$filepath[i])
            
            myData <- read.fwf(gz, widths = c(7, 6, 12, 6, 3, 8, 3, 8, 
                                              3, 8, 3, 7, 3, 7, 3, 7, 
                                              7, 9, 8, 6, 7, 7),
                               col.names=c("STN","WBAN","YEARMODA","TEMP","TEMP2",
                                           "DEWP","DEWP2","SLP", "SLP2", "STP", "STP2",
                                           "VISIB", "VISIB2", "WDSP", "WDSP2", "MXSPD",
                                           "GUST", "MAX", "MIN", "PRCP", "SNDP", "FRSHTT"),
                               strip.white=TRUE, skip=1)
            
            ### extract data based on date.of.interest
            gsodDF$selected.wind.speed[i] <- myData$MXSPD[myData$YEARMODA == date.of.interest] 
            
            
        } else {
            print("file not found")
        }
        
    } # i
    
    
    ### remove NAs
    subDF <- gsodDF[complete.cases(gsodDF$selected.wind.speed),]
    
    ### check severity
    subDF$wind_severity_percentile <- ifelse(subDF$selected.wind.speed>=subDF$wind999, 99.9,
                                             ifelse(subDF$selected.wind.speed>=subDF$wind99 & subDF$selected.wind.speed<subDF$wind999, 99.0,
                                                    ifelse(subDF$selected.wind.speed>=subDF$wind95 & subDF$selected.wind.speed<subDF$wind99, 95.0,
                                                           ifelse(subDF$selected.wind.speed>=subDF$wind90 & subDF$selected.wind.speed<subDF$wind95, 90.0,
                                                                  ifelse(subDF$selected.wind.speed>=subDF$wind80 & subDF$selected.wind.speed<subDF$wind90, 80.0,
                                                                         ifelse(subDF$selected.wind.speed>=subDF$wind70 & subDF$selected.wind.speed<subDF$wind80, 70.0,
                                                                                ifelse(subDF$selected.wind.speed>=subDF$wind60 & subDF$selected.wind.speed<subDF$wind70, 60.0,
                                                                                       ifelse(subDF$selected.wind.speed>=subDF$wind50 & subDF$selected.wind.speed<subDF$wind60, 50.0,
                                                                                              40.0))))))))
    
    
    ### save data
    write.csv(subDF, paste0(destDir, "/GSOD_Wind_Extreme_Severity_", date.of.interest, "_", 
                            user.region.name,
                            "_regions.csv"))
    

}