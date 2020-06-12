convert_from_spatial_to_temporal_DF <- function(sourceDir, destDir) {
    
    #### Create output folder
    if(!dir.exists(destDir)) {
        dir.create(destDir, showWarnings = FALSE)
    }
    
    ### prepare year DFs
    yr.list <- c(1900:2019)
    n.yr <- length(yr.list)
    
    ### number of leap years
    lp.year <- 29
    n.days <- 29 + n.yr * 365 + 31 + 29 + 31
    
    ### grid information
    lat.id <- c(1:691)
    lat.lab <- paste0("lat", lat.id)
    
    lon.id <- c(1:886)
    lon.lab <- paste0("lon", lon.id)
    
    lon <- seq(111.975, 111.975 + (0.05 * 885), by=0.05)
    lat <- seq(-44.525, -44.525 + (0.05 * 690), by=0.05)
    
    ### create lon lat DF for future plotting
    latlonDF <- data.frame(rep(lat.id, each = max(lon.id)),
                           rep(lon.id, max(lat.id)), 
                           rep(lat, each = max(lon.id)),
                           rep(lon, max(lat.id)))
    colnames(latlonDF) <- c("latID", "lonID", "lat", "lon")
    
    ### add group information to split the DF to make it smaller
    latlonDF$Group <- c(rep(c(1:23), each = 886 * 30), 
                        rep(23, each=886 * 1))
    
    ### prepare all input file path
    dayDF <- data.frame(seq.Date(as.Date("1900/01/01"), 
                                 as.Date("2020/03/31"), 
                                 by="day"),
                        NA, NA, NA)
    colnames(dayDF) <- c("Date", "Year", "Lab", "Path")
    dayDF$Year <- year(dayDF$Date)
    dayDF$Lab <- gsub("-", "", dayDF$Date)
    dayDF$Path <- paste0(sourceDir, dayDF$Year, "/rain_", 
                         dayDF$Lab, ".grid")
    
    #### To process the raw data into groupped output
    for (i in c(1:23)) {
        
        ### get subset lat information
        lat.sub <- subset(latlonDF, Group == i)
        lat.list.sub <- unique(lat.sub$latID)
        lat.length <- length(lat.list.sub)
        
        ### create out storage matrix
        out <- array(NA, c(lat.length, 886, n.days))
        
        ### read in data
        for (j in 1:n.days) {
            
            ## read in data
            inName <- dayDF[j,"Path"]
            myDF <- read.ascii.grid(inName)
            
            for (k in lat.list.sub) {
                ### get small k information
                k2 <- k - 30 * (i - 1)
                
                ### save data
                out[k2, , j] <- myDF$data[k,]
            }  # k loop
        }      # j loop
        
        ### save output
        saveRDS(out, file=paste0(destDir, "/Group_", i, ".rds"))
    }   # i loop
    
    
    
}   # function loop


