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
    
    tmp <- c()
    
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
        pathDF <- data.frame(c(s.year:e.year), NA)
        colnames(pathDF) <- c("year", "path")
        
        ## folder path
        pathDF$path <- paste0(sourceDir, "/gsod/gsod", pathDF$year, "/",
                              station.id, "-", pathDF$year, ".op")
        
        ## to do next:
        ## read in the path and create percentile dataframe on wind and gust data
        
        
    }

    ### loop through the folders to find stations based on the user defined station list
    for (i in c(1929:2020)) {
        inPath <- paste0(sourceDir, "/gsod/gsod_", i, "/")
    }
    
    ### create data path
    gsodDF.path <- create_data_path_for_GSOD_dataset(inDF=gsodDF)
    
}