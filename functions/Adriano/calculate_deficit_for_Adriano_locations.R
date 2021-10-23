calculate_deficit_for_Adriano_locations <- function(siteDF) {
    
    ### read input
    myDF <- readRDS("input/Adriano/pd_Adriano_sites.rds")
    
    
    ### convert rain daily data into monthly data
    ### prepare all input file path
    dayDF <- data.frame(seq.Date(as.Date("1971/01/01"), 
                                 as.Date("2020/11/30"), 
                                 by="month"),
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
    out <- array(NA, c(15, n.yr))
    
    ### Calculate annual total
    for (i in 1:n.yr) {
        s <- indexDF$s[i]
        e <- indexDF$e[i]
        sub <- myDF[,s:e]
        annual <- rowSums(sub, na.rm=T)
        out[,i] <- annual
    }
    
    
    ### remove the last year where there is only 11 months
    outDF <- out[,1:49]
    
    
    ### calculate MAP
    mapDF <- rowMeans(outDF, na.rm=T)
    
    
    ### calculate MAP in the two year before jan 2020
    subDF <- out[,48:49]
    
    ### calculate MAP
    mapDF2 <- rowMeans(subDF, na.rm=T)
    
    ### assign
    siteDF$PD <- mapDF
    
    siteDF$PD_2yr_to_2020 <- mapDF2
    
    return(siteDF)
    
}



    