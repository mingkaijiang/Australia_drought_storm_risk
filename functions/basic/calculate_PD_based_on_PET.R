calculate_PD_based_on_PET <- function (sourceDir,
                                       destDir,
                                       varName,
                                       user.lat.max,
                                       user.lat.min,
                                       user.lon.max,
                                       user.lon.min,
                                       user.region.name) {
    
    
    ### read in the PET monthly dataframe
    pet <- readRDS(paste0(sourceDir, "/pet_", user.region.name, "_regions.rds"))
    
    ### read in rain daily
    rain <- readRDS(paste0(sourceDir, "/rain_", user.region.name, "_regions.rds"))
    
    ### convert rain daily data into monthly data
    ### prepare all input file path
    dayDF <- data.frame(seq.Date(as.Date("1900/01/01"), 
                                 as.Date("2020/03/31"), 
                                 by="day"),
                        NA, NA, NA, NA)
    colnames(dayDF) <- c("Date", "Year", "Month", "YearMonth", "loc")
    dayDF$Year <- year(dayDF$Date)
    dayDF$Month <- month(dayDF$Date)
    dayDF$YearMonth <- substr(dayDF$Date, 1, 7)
    dayDF$loc <- c(1:nrow(dayDF))
    
    ### ignore earlier dates
    #dayDF <- subset(dayDF, Date >= "1911-01-01" & Date <= "2020-03-30")
    dayDF <- subset(dayDF, Date >= "1911-01-01")
    
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
        sub <- rain[,,s:e]
        monthly <- rowSums(sub, dims = 2, na.rm=T)
        out[,,i] <- monthly[,]
    }
    
    ### actual precip
    act <- out
    dims <- dim(act)
    dim(act) <- c(prod(dims[1:2]), dims[3])
    
    ### calculate actual - potential for each month
    diff <- act - pet
    
    ### save output
    saveRDS(diff, file=paste0(destDir, "/", varName, "_", user.region.name, "_regions.rds"))
    
}