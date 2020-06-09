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
    
    latlonDF <- data.frame(rep(lat.id, each = max(lon.id)),
                           rep(lon.id, max(lat.id)), 
                           rep(lat, each = max(lon.id)),
                           rep(lon, max(lat.id)))
    colnames(latlonDF) <- c("latID", "lonID", "lat", "lon")
    
    
    ### Prepare all year output array
    out <- c()
    
    #### To process the raw data into format easily readable
    for (i in yr.list) {
        ### complete the path
        sDir <- paste0(sourceDir, i)

        ### Source all files in input folder
        DatFiles <- list.files(path = sDir, pattern = "\\.grid")
        
        ### Prepare output array
        if (leap_year(i)) {
            daily.tmp <- array(NA, c(691, 886, 366))
        } else {
            daily.tmp <- array(NA, c(691, 886, 365))
        }
        
        ### Read in data
        for (j in 1:length(DatFiles)) {
            inName <- file.path(sDir, DatFiles[j], fsep = .Platform$file.sep)
            myDF <- read.ascii.grid(inName)
            
            daily.tmp[, , j] <- myDF$data
        }   
        
        
        saveRDS(out, file=paste0(destDir, "/DF", i, ".rds"))
        
        
    }  # i loop
}   # function loop


