calculate_PET_based_on_Tmax <- function (sourceDir,
                                         destDir,
                                         varName,
                                         user.lat.max,
                                         user.lat.min,
                                         user.lon.max,
                                         user.lon.min,
                                         user.region.name) {
    
    ### read in the R database
    myData <- readRDS(paste0(sourceDir, "/tmax_", user.region.name, "_regions.rds"))
    
    dim1 <- dim(myData)[1]
    dim2 <- dim(myData)[2]
    dim3 <- dim(myData)[3]
        
    ### full lat and lon list
    lat <- seq(-10.025, -10.025 + (-0.05 * 690), by=-0.05)
    lon <- seq(111.975, 111.975 + (0.05 * 885), by=0.05)
    
    ### subset
    lat <- lat[lat <= user.lat.max]
    lat <- lat[lat >= user.lat.min]
    
    lon <- lon[lon <= user.lon.max]
    lon <- lon[lon >= user.lon.min]
    
    latlonDF <- data.frame(rep(c(1:dim1), each = dim2),
                           rep(c(1:dim2), dim1), 
                           rep(lat, each = dim2),
                           rep(lon, dim1))
    colnames(latlonDF) <- c("latID", "lonID", "lat", "lon")
    
    ### prepare lat list
    lat.full <- rep(latlonDF$lat, dim3)
    
    Tave.full <- melt(myData)
    
    ### use SPEI function thornthwaite to compute PET
    test <- thornthwaite(Tave=myData, lat = lat.full, na.rm=F)
    
     ### save output
    saveRDS(out, file=paste0(destDir, "/", varName, "_", user.region.name, "_regions.rds"))
    
}