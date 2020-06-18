process_GSOD_station_data <- function(sourceDir, destDir,
                                      user.region.name,
                                      user.lat.max,
                                      user.lat.min,
                                      user.lon.max,
                                      user.lon.min,
                                      plot.option) {
    
    
    ### select station list based on user defined region
    gsodDF <- select_station_from_GSOD_global_dataset(destDir=destDir,
                                                      user.region.name=user.region.name,
                                                      user.lat.max=user.lat.max,
                                                      user.lat.min=user.lat.min,
                                                      user.lon.max=user.lon.max,
                                                      user.lon.min=user.lon.min,
                                                      plot.option=plot.option)
    

    ### number of stations
    n.station <- dim(gsodDF)[1]
    
    ### loop through each station
    for (i in c(1:n.station)) {
        
        ## get start and end year
        s.year <- gsodDF$s.year[i]
        e.year <- gsodDF$e.year[i]
        n.year <- length(c(s.year:e.year))
        
        ## station id
        station.id <- paste0(gsodDF$USAF[i], "-", gsodDF$WBAN[i])
        
        ## create DF to store paths
        pathDF <- data.frame(c(s.year:e.year), NA, NA, NA)
        colnames(pathDF) <- c("year", "path", "file", "filepath")
        
        ## folder path
        pathDF$file <- paste0(station.id, "-", pathDF$year, ".op.gz")
        
        pathDF$path <- paste0(sourceDir, "gsod_", pathDF$year, "/")
        
        pathDF$filepath <- paste0(sourceDir, "gsod_", pathDF$year, "/",
                                  station.id, "-", pathDF$year, ".op.gz")
        
        ## read in the path and create percentile dataframe on wind and gust data
        n.files <- dim(pathDF)[1]
        
        ## empty array to store data
        tmp1 <- c()
        tmp2 <- c()
        
        for (j in 1:n.files) {
            all.file.names <- list.files(path = pathDF$path[j], pattern = "*.op.gz")
            
            ## check of the file exists
            if(pathDF$file[j] %in% all.file.names) {
                
                ## print file found
                print("file found")
                
                ## read in data
                gz <- gzfile(pathDF$filepath[j])
                
                #gz <- gzfile("input/gsod/gsod_2001/010010-99999-2001.op.gz")
                myData <- read.fwf(gz, widths = c(7, 6, 12, 6, 3, 8, 3, 8, 
                                                      3, 8, 3, 7, 3, 7, 3, 7, 
                                                      7, 9, 8, 6, 7, 7),
                                   col.names=c("STN","WBAN","YEARMODA","TEMP","TEMP2",
                                               "DEWP","DEWP2","SLP", "SLP2", "STP", "STP2",
                                               "VISIB", "VISIB2", "WDSP", "WDSP2", "MXSPD",
                                               "GUST", "MAX", "MIN", "PRCP", "SNDP", "FRSHTT"),
                                   strip.white=TRUE, skip=1)
                
                ### add data
                tmp1 <- c(tmp1, myData$MXSPD)
                tmp2 <- c(tmp2, myData$GUST)
                
            } else {
                print("file not found")
                #tmp1 <- tmp1
                #tmp2 <- tmp2
            }
            
        } # j
        
        ### replace 999.9 with NA
        tmp1 <- as.numeric(gsub(999.9, "", tmp1))
        tmp2 <- as.numeric(gsub(999.9, "", tmp2))
        
        ### assign value
        gsodDF$max.wind[i] <- ifelse(length(tmp1) == 0, NA, max(tmp1, na.rm=T))
        gsodDF$wind999[i] <- ifelse(length(tmp1) == 0, NA, quantile(tmp1, 0.999, na.rm=T))
        gsodDF$wind99[i] <- ifelse(length(tmp1) == 0, NA, quantile(tmp1, 0.99, na.rm=T))
        gsodDF$wind95[i] <- ifelse(length(tmp1) == 0, NA, quantile(tmp1, 0.95, na.rm=T))
        gsodDF$wind90[i] <- ifelse(length(tmp1) == 0, NA, quantile(tmp1, 0.90, na.rm=T))
        gsodDF$wind80[i] <- ifelse(length(tmp1) == 0, NA, quantile(tmp1, 0.80, na.rm=T))
        gsodDF$wind70[i] <- ifelse(length(tmp1) == 0, NA, quantile(tmp1, 0.70, na.rm=T))
        gsodDF$wind60[i] <- ifelse(length(tmp1) == 0, NA, quantile(tmp1, 0.60, na.rm=T))
        gsodDF$wind50[i] <- ifelse(length(tmp1) == 0, NA, quantile(tmp1, 0.50, na.rm=T))
        gsodDF$wind40[i] <- ifelse(length(tmp1) == 0, NA, quantile(tmp1, 0.40, na.rm=T))
        
        gsodDF$max.gust[i] <- ifelse(length(tmp2) == 0, NA, max(tmp2, na.rm=T))
        gsodDF$gust999[i] <- ifelse(length(tmp2) == 0, NA, quantile(tmp2, 0.999, na.rm=T))
        gsodDF$gust99[i] <- ifelse(length(tmp2) == 0, NA, quantile(tmp2, 0.99, na.rm=T))
        gsodDF$gust95[i] <- ifelse(length(tmp2) == 0, NA, quantile(tmp2, 0.95, na.rm=T))
        gsodDF$gust90[i] <- ifelse(length(tmp2) == 0, NA, quantile(tmp2, 0.90, na.rm=T))
        gsodDF$gust80[i] <- ifelse(length(tmp2) == 0, NA, quantile(tmp2, 0.80, na.rm=T))
        gsodDF$gust70[i] <- ifelse(length(tmp2) == 0, NA, quantile(tmp2, 0.70, na.rm=T))
        gsodDF$gust60[i] <- ifelse(length(tmp2) == 0, NA, quantile(tmp2, 0.60, na.rm=T))
        gsodDF$gust50[i] <- ifelse(length(tmp2) == 0, NA, quantile(tmp2, 0.50, na.rm=T))
        gsodDF$gust40[i] <- ifelse(length(tmp2) == 0, NA, quantile(tmp2, 0.40, na.rm=T))
        
    } # i

    ### save output
    write.csv(gsodDF, paste0(destDir, "/GSOD_Wind_Extreme_", user.region.name, "_regions.csv"),
              row.names=F)
    
    ### to do next:
    ### 1. add % of data so that we know how many data point there is
    ### 2. plot maps to see patterns to make sure the code processing is trustworthy
    ### 3. develop code to extract date of interest wind speed, then we can compare against extreme index
    ### 4. add plots to the gridded plots
    ### 5. clean/redevelop code to speed up
    ### 6. gridded wind data is needed
    
    
}