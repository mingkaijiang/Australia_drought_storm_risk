calculate_MAP_in_2020_for_Adirano_locations <- function(siteDF) {
    
    ### read input
    myDF <- readRDS("input/Adriano/rain_Adriano_sites.rds")
    
    
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
        sub <- myDF[,s:e]
        monthly <- rowSums(sub, na.rm=T)
        out[,i] <- monthly
    }
    
    
    ### subset the last 11 months
    outDF <- out[,589:599]
    
    
    ### calculate MAP
    mapDF <- rowSums(outDF, na.rm=T)
    
    ### assign
    siteDF$MAP_in_2020 <- mapDF
    
    return(siteDF)
    
}



    