calculate_PET_based_on_Tmax_Adriano <- function (sourceDir,
                                                 destDir,
                                                 varName,
                                                 siteDF,
                                                 user.region.name) {
    
    ### read in the R database
    myData <- readRDS(paste0(sourceDir, "/tmax_", user.region.name, "_sites.rds"))
    
    dim1 <- dim(myData)[1]
    dim2 <- dim(myData)[2]
    dim3 <- dim(myData)[3]
    
    
    ### prepare all input file path
    dayDF <- data.frame(seq.Date(as.Date("1950/01/01"), 
                                 as.Date("2020/11/30"), 
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
    
    
    ### prepare storageDF for PET
    pet <- array(NA, c(dim1*dim2, n.month))
    dim4 <- dim(pet)[1]
    
    test <- out
    dims <- dim(test)
    dim(test) <- c(prod(dims[1:2]), dims[3])
    
    for (i in 1:dim4) {
        lat <- siteDF$Lat[i]
        Tave <- test[i,]
        pet[i,] <- thornthwaite(Tave, lat, na.rm=T)
    }
    
    ### save output
    saveRDS(pet, file=paste0(destDir, "/", varName, "_", user.region.name, "_sites.rds"))
    
}