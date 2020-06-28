calculate_PET_based_on_Tmax <- function (sourceDir,
                                         destDir,
                                         varName,
                                         user.lat.max,
                                         user.lat.min,
                                         user.lon.max,
                                         user.lon.min,
                                         user.region.name) {
    
    sourceDir = "input"
    destDir = "input"
    varName = "pet"
    user.lat.max = -31
    user.lat.min = -35
    user.lon.max = 153
    user.lon.min = 149
    user.region.name = "SydneyHunter"
    
    ### read in the R database
    myData <- readRDS(paste0(sourceDir, "/tmax_", user.region.name, "_regions.rds"))
    
    dim1 <- dim(myData)[1]
    dim2 <- dim(myData)[2]
    dim3 <- dim(myData)[3]
    
    
    ### prepare all input file path
    dayDF <- data.frame(seq.Date(as.Date("1911/01/01"), 
                                 as.Date("2020/03/30"), 
                                 by="day"),
                        NA, NA, NA, NA)
    colnames(dayDF) <- c("Date", "Year", "Month", "YearMonth", "loc")
    dayDF$Year <- year(dayDF$Date)
    dayDF$Month <- month(dayDF$Date)
    dayDF$YearMonth <- substr(dayDF$Date, 1, 7)
    dayDF$loc <- c(1:nrow(dayDF))
    
    ### prepare index file
    indexDF <- data.frame(unique(dayDF$YearMonth), NA, NA)
    colnames(indexDF) <- c("YearMonth", "s", "e")
    indexDF$YearMonth <- as.character(indexDF$YearMonth)
    
    for (i in indexDF$YearMonth) {
        subDF <- subset(dayDF, YearMonth==i)
        s <- min(subDF$loc)
        e <- max(subDF$loc)
        indexDF$s[indexDF$YearMonth == i] <- s
        indexDF$e[indexDF$YearMonth == i] <- e
    }
    
    ### prepare a storage DF for monthly average Tmax
    n.month <- dim(indexDF)[1]
    out <- array(NA, c(dim1, dim2, n.month))
    
    ### Calculate monthly mean
    for (i in 1:n.month) {
        s <- indexDF$s[i]
        e <- indexDF$e[i]
        sub <- myData[,,s:e]
        monthly <- rowMeans(sub, dims = 2, na.rm=T)
        out[,,i] <- monthly[,]
    }
    
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
    
    
    ### prepare storageDF for PET
    pet <- array(NA, c(dim1*dim2, n.month))
    dim4 <- dim(pet)[1]
    
    test <- out
    dims <- dim(test)
    dim(test) <- c(prod(dims[1:2]), dims[3])
    
    for (i in 1:dim4) {
        lat <- latlonDF.sub$lat[i]
        Tave <- test[i,]
        pet[i,] <- thornthwaite(Tave, lat, na.rm=T)
    }
    
    ### save output
    saveRDS(pet, file=paste0(destDir, "/", varName, "_", user.region.name, "_regions.rds"))
    
}