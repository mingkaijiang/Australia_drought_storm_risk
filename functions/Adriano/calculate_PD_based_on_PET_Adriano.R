calculate_PD_based_on_PET_Adriano <- function (sourceDir,
                                               destDir,
                                               varName,
                                               siteDF,
                                               user.region.name) {
    
    
    ### read in the PET monthly dataframe
    pet <- readRDS(paste0(sourceDir, "/pet_", user.region.name, "_sites.rds"))
    
    ### read in rain daily
    rain <- readRDS(paste0(sourceDir, "/rain_", user.region.name, "_sites.rds"))
    
    ### convert rain daily data into monthly data
    ### prepare all input file path
    dayDF <- data.frame(seq.Date(as.Date("1971/01/01"), 
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
    out <- array(NA, c(15, n.month))
    
    ### Calculate monthly mean
    for (i in 1:n.month) {
        s <- indexDF$s[i]
        e <- indexDF$e[i]
        sub <- rain[,s:e]
        monthly <- rowSums(sub, na.rm=T)
        out[,i] <- monthly
    }
    
    ### actual precip
    act <- out
    
    ### calculate actual - potential for each month
    diff <- act - pet
    
    ### save output
    saveRDS(diff, file=paste0(destDir, "/", varName, "_", user.region.name, "_sites.rds"))
    
}