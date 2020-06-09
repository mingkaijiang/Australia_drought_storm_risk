compute_storm_index <- function(sourceDir, destDir) {
    
    ### prepare year DFs
    yr.list <- c(1900:2018)
    n.yr <- length(yr.list)
    
    ### number of leap years
    lp.year <- 29
    n.days <- 29 * 30 + n.yr * 365
    
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
    latlonDF$Group <- c(rep(c(1:100), each = 6122), 
                        rep(101, each=26))
    
    ### prepare all input file path
    dayDF <- data.frame(seq.Date(as.Date("1900/01/01"), 
                                 as.Date("2018/12/31"), 
                                 by="day"),
                        NA, NA, NA)
    colnames(dayDF) <- c("Date", "Year", "Lab", "Path")
    dayDF$Year <- year(dayDF$Date)
    dayDF$Lab <- gsub("-", "", dayDF$Date)
    dayDF$Path <- paste0(sourceDir, dayDF$Year, "/rain_", 
                         dayDF$Lab, ".grid")
    
    ### Prepare all year output array
    out <- c()
    
    #### To process the raw data into format easily readable
    for (i in c(1:101)) {
        
        ### create storage matrix
        group.length <- dim(latlonDF[latlonDF$Group==i,])[1]
        out <- array(NA, c(691, 886, group.length))
        
        ### read in data
        day.length <- dim(dayDF)[1]
        for (j in day.length) {
            inName <- dayDF[j,"Path"]
            myDF <- read.ascii.grid(inName)
            
            daily.tmp[, , j] <- myDF$data
        }
        
        
        ### save output
        saveRDS(out, file=paste0(destDir, "/Group_", i, ".rds"))
    }
    
    
    
    for (i in lat.id) {
        for (j in lon.id) {
            
           
            ### Read in data
            for (j in 1:length(DatFiles)) {
                inName <- file.path(sDir, DatFiles[j], fsep = .Platform$file.sep)
                myDF <- read.ascii.grid(inName)
                
                daily.tmp[, , j] <- myDF$data
            }   
            
            
            saveRDS(out, file=paste0(destDir, "/DF", i, ".rds"))
            
        } # j loop
        
        
    }  # i loop
}   # function loop


