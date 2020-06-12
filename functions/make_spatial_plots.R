make_spatial_plots <- function(sourceDir, destDir,
                               inFile,
                               date.of.interest,
                               storm.duration,
                               drought.duration) {
    
    #### Create output folder
    if(!dir.exists(destDir)) {
        dir.create(destDir, showWarnings = FALSE)
    }
    
    #### read in data
    storm.intensity <- readRDS(paste0(sourceDir, 
                                      "/storm_intensity_", 
                                      date.of.interest, "_",
                                      storm.duration, "_",
                                      inFile))
    
    drought.intensity <- readRDS(paste0(sourceDir, 
                                      "/drought_intensity_", 
                                      date.of.interest, "_",
                                      drought.duration, "_",
                                      inFile))
    
    storm.severity <- readRDS(paste0(sourceDir, 
                                      "/storm_severity_", 
                                      date.of.interest, "_",
                                      storm.duration, "_",
                                      inFile))
    
    drought.severity <- readRDS(paste0(sourceDir, 
                                     "/drought_severity_", 
                                     date.of.interest, "_",
                                     drought.duration, "_",
                                     inFile))
    
    
    ### prepare lat and lon real information
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
    latlonDF.sub <- latlonDF[latlonDF$lat<=-35 & latlonDF$lat >= -45 & latlonDF$lon <= 155 & latlonDF$lon>=150,]
    
    ### get subset lat information
    lat.list.sub <- unique(latlonDF.sub$latID)
    lat.length <- length(lat.list.sub)
    lat.id <- 1:lat.length
    
    lon.list.sub <- unique(latlonDF.sub$lonID)
    lon.length <- length(lon.list.sub)
    lon.id <- 1:lon.length
    
    ### Make spatial plots
    plot(raster(drought.severity))
    plot(raster(storm.severity))
    
}