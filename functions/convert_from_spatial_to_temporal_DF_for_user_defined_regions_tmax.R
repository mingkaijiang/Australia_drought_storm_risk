convert_from_spatial_to_temporal_DF_for_user_defined_regions_tmax <- function(sourceDir, destDir,
                                                                              varName,
                                                                              user.lat.min,
                                                                              user.lat.max,
                                                                              user.lon.min,
                                                                              user.lon.max,
                                                                              user.region.name) {
    
    #### Create output folder
    if(!dir.exists(destDir)) {
        dir.create(destDir, showWarnings = FALSE)
    }
    
    ### prepare year DFs
    yr.list <- c(1911:2019)
    n.yr <- length(yr.list)
    
    ### number of leap years
    lp.year <- 26
    n.days <- 26 + n.yr * 365 + 31 + 29 + 31
    
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
    latlonDF.sub <- latlonDF[latlonDF$lat<=user.lat.max & latlonDF$lat >= user.lat.min & 
                                 latlonDF$lon <= user.lon.max & latlonDF$lon>=user.lon.min,]
    
    ### prepare all input file path
    dayDF <- data.frame(seq.Date(as.Date("1911/01/01"), 
                                 as.Date("2020/03/31"), 
                                 by="day"),
                        NA, NA, NA)
    colnames(dayDF) <- c("Date", "Year", "Lab", "Path")
    dayDF$Year <- year(dayDF$Date)
    dayDF$Lab <- gsub("-", "", dayDF$Date)
    
    if (varName == "rain") {
        dayDF$Path <- paste0(sourceDir, dayDF$Year, "/rain_", 
                             dayDF$Lab, ".grid")
    } else if (varName == "tmax") {
        dayDF$Path <- paste0(sourceDir, dayDF$Year, "/", dayDF$Lab,
                             dayDF$Lab, ".grid")
    } else if (varName == "vp3pm") {
        dayDF$Path <- paste0(sourceDir, dayDF$Year, "/", dayDF$Lab,
                             dayDF$Lab, ".grid")
    } else {
        print(paste0("no rule for ", varName))
    }
    
    
    ### get subset lat information
    lat.list.sub <- unique(latlonDF.sub$latID)
    lat.length <- length(lat.list.sub)
    lat.id <- 1:lat.length
    
    lon.list.sub <- unique(latlonDF.sub$lonID)
    lon.length <- length(lon.list.sub)
    lon.id <- 1:lon.length
    
    ### create out storage matrix
    out <- array(NA, c(lat.length, lon.length, n.days))
    
    ### read in data
    for (i in 1:n.days) {
        
        ## read in data
        inName <- dayDF[i,"Path"]
        myDF <- read.ascii.grid(inName)
        
        for (j in lat.id) {
            for (k in lon.id) {
                j2 <- lat.list.sub[j]
                k2 <- lon.list.sub[k]
                ### save data
                out[j, k, i] <- myDF$data[j2,k2]
            } # k loop
        } # j loop
    } # i loop
    
    ### save output
    saveRDS(out, file=paste0(destDir, "/", varName, "_", user.region.name, "_regions.rds"))
    
}   # function loop


