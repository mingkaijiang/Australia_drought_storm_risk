read_in_sites_and_convert_to_AWAP <- function() {
    
    ### read in sites
    siteDF <- read.csv("input/Adriano_Sites.csv")
    colnames(siteDF) <- c("Region", "Site", "Site_Lat", "Site_Lon")
    
    ### split the lon and lat
    siteDF$lat1 <- substr(siteDF$Site_Lat, 1, 5)
    siteDF$lat2 <- round(as.numeric(paste0("-0.0", 
                                           substr(siteDF$Site_Lat, 6, 
                                                  length(siteDF$Site_Lat)))), 3)
    
    siteDF$lon1 <- substr(siteDF$Site_Lon, 1, 5)
    siteDF$lon2 <- round(as.numeric(paste0("0.0", 
                                           substr(siteDF$Site_Lon, 6, 
                                                  length(siteDF$Site_Lon)))), 3)
    
    
    ### round to the nearest 0.05
    siteDF$lat3 <- ifelse(siteDF$lat2>=-0.025, -0.025, 
                          ifelse(siteDF$lat2<(-0.075), -0.125, -0.075))
    
    siteDF$lon3 <- ifelse(siteDF$lon2<0.025, -0.025, 
                          ifelse(siteDF$lon2>=0.075, 0.075, 0.025))
    
    
    siteDF$lat4 <- as.numeric(siteDF$lat1)+siteDF$lat3
    siteDF$lon4 <- as.numeric(siteDF$lon1)+siteDF$lon3
    
    
    ### remove
    siteDF$lat1 <- NULL
    siteDF$lat2 <- NULL
    siteDF$lat3 <- NULL
    
    siteDF$lon1 <- NULL
    siteDF$lon2 <- NULL
    siteDF$lon3 <- NULL
    
    names(siteDF)[names(siteDF)=="lat4"] <- "AWAP_Lat"
    names(siteDF)[names(siteDF)=="lon4"] <- "AWAP_Lon"
    
    
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
    
    
    
    ### merge and find location
    myDF <- merge(siteDF, latlonDF, by.x=c("AWAP_Lat", "AWAP_Lon"),
                  by.y=c("lat", "lon"), all.x=T)
    
    
    
    names(myDF)[names(myDF)=="AWAP_Lat"] <- "Lat"
    names(myDF)[names(myDF)=="AWAP_Lon"] <- "Lon"
    
    ### order
    myDF <- myDF[,c("Lat", "Lon", "latID", "lonID", "Site_Lat", "Site_Lon",
                    "Site", "Region")]
    
    
    return(myDF)

}

