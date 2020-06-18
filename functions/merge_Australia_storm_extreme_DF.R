merge_Australia_storm_extreme_DF <- function(sourceDir, destDir, 
                                             duration) {
    
    
    ### prepare storage DF
    stDF <- array(NA, c(691, 886, 5))
    
    ### allocate splitted data to the whole dataset
    for (i in 1:23) {
        
        ### read in the data
        myData <- readRDS(paste0(sourceDir, "/Group_", i, 
                                 "_Storm_extreme_percentile_", duration,
                                 "_Australia.rds"))
        
        dim <- dim(myData)[1]
        
        ### location index
        loc1 <- (i-1)*30 + 1
        loc2 <- loc1 + dim - 1
        
        ### assign values
        stDF[loc1:loc2,,] <- myData
    }
    
    ### save
    saveRDS(stDF, file=paste0(destDir, "/Storm_extreme_percentile_", duration,
                                        "_Australia.rds"))
    
    ########################### prepare grid information DF ############################
    ### grid information
    lat.id <- c(1:691)
    lat.lab <- paste0("lat", lat.id)
    
    lon.id <- c(1:886)
    lon.lab <- paste0("lon", lon.id)
    
    lon <- seq(111.975, 111.975 + (0.05 * 885), by=0.05)
    lat <- seq(-10.025, -10.025 + (-0.05 * 690), by=-0.05)
    
    ### create lon lat DF for future plotting
    latlonDF <- data.frame(rep(lat.id, each = max(lon.id)),
                           rep(lon.id, max(lat.id)), 
                           rep(lat, each = max(lon.id)),
                           rep(lon, max(lat.id)))
    colnames(latlonDF) <- c("latID", "lonID", "lat", "lon")
    
    ### add group information to split the DF to make it smaller
    latlonDF$Group <- c(rep(c(1:23), each = 886 * 30), 
                        rep(23, each=886 * 1))
    
    ########################### end grid information DF ############################
    
}