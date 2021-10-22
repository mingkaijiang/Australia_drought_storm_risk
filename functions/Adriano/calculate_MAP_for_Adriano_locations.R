calculate_MAP_for_Adirano_locations <- function() {
    
    ### read input
    myDF <- readRDS("input/Adriano/rain_Adriano_sites.rds")
    
    
    ### convert rain daily data into monthly data
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
    indexDF <- data.frame(unique(dayDF$Year), NA, NA)
    colnames(indexDF) <- c("Year", "s", "e")
    indexDF$Year <- as.character(indexDF$Year)
    
    for (i in indexDF$Year) {
        subDF <- subset(dayDF, Year==i)
        s <- min(subDF$loc)
        e <- max(subDF$loc)
        indexDF$s[indexDF$Year == i] <- s
        indexDF$e[indexDF$Year == i] <- e
    }
    
    ### prepare a storage DF for monthly total
    n.yr <- dim(indexDF)[1]
    out <- array(NA, c(dim1, dim2, n.yr))
    
    ### Calculate annual total
    for (i in 1:n.yr) {
        s <- indexDF$s[i]
        e <- indexDF$e[i]
        sub <- myDF[,,s:e]
        annual <- rowSums(sub, dims = 2, na.rm=T)
        out[,,i] <- annual[,]
    }
    
    
    ### remove the last year where there is only 11 months
    outDF <- out[,,1:70]
    
    
    ### calculate MAP
    
    
}



    