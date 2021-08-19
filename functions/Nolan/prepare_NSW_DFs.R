prepare_NSW_DFs <- function() {
    
    
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
    
    
    ### prepare NSW range
    nsw.lon.max = 156
    nsw.lon.min = 140
    nsw.lat.max = -28
    nsw.lat.min = -38
    
    nsw.lat.list.s <- seq(nsw.lat.max, (nsw.lat.min+5), by=-5)
    nsw.lon.list.s <- seq(nsw.lon.min, (nsw.lon.max-8), by=8)
    nsw.lat.list.e <- seq((nsw.lat.max-5), nsw.lat.min, by=-5)
    nsw.lon.list.e <- seq((nsw.lon.min+8), nsw.lon.max, by=8)
    
    nswDF <- data.frame("lon.start" = rep(nsw.lon.list.s, each=length(nsw.lat.list.s)),
                        "lon.end" = rep(nsw.lon.list.e, each=length(nsw.lat.list.e)),
                        "lat.start" = rep(nsw.lat.list.s, length(nsw.lon.list.s)),
                        "lat.end" = rep(nsw.lat.list.e, length(nsw.lon.list.e)))
    
    
    n <- nrow(nswDF)
    
    for (i in 1:n) {
        latlonDF.sub <- latlonDF[latlonDF$lat<=nswDF$lat.start[i] & 
                                     latlonDF$lat >= nswDF$lat.end[i] & 
                                     latlonDF$lon <= nswDF$lon.end[i] & 
                                     latlonDF$lon >= nswDF$lon.start[i],]
        
        
        latID.range <- range(latlonDF.sub$latID)
        lonID.range <- range(latlonDF.sub$lonID)
        
        nswDF$latID.start[i] <- min(latID.range)
        nswDF$latID.end[i] <- max(latID.range)
        
        nswDF$lonID.start[i] <- min(lonID.range)
        nswDF$lonID.end[i] <- max(lonID.range)
        
    }
    
    return(nswDF)

}

