compute_antecedent_water_deficit_severity_for_user_defined_regions <- function(sourceDir, 
                                                                               destDir, 
                                                                               user.lat.min,
                                                                               user.lat.max,
                                                                               user.lon.min,
                                                                               user.lon.max,
                                                                               user.region.name,
                                                                               date.of.interest,
                                                                               duration) {
    
 
    ### translate date of interest into nday information
    dateDF <- data.frame(seq.Date(as.Date("1911/01/01"), 
                                     as.Date("2020/03/31"), 
                                     by="day"), NA, NA, NA, NA)
    colnames(dateDF) <- c("Date", "YearMonth", "Year", "Month", "Date2")
    dateDF$Date2 <- gsub("-", "", dateDF$Date)
    dateDF$Year <- year(dateDF$Date)
    dateDF$Month <- month(dateDF$Date)
    dateDF$YearMonth <- substr(dateDF$Date, 1, 7)
    
    monthDF <- summaryBy(Year+Month~YearMonth, data=dateDF, FUN=unique, keep.names=T)
    
    ### convert date of interest to month of interest                     
    month.of.interest <- paste0(substr(date.of.interest, 1, 4), "-",
                                substr(date.of.interest, 5, 6))
    
    
    n.index <- match(month.of.interest, monthDF$YearMonth)
    
    ### print statement
    if (is.na(n.index)) {
        stop(paste0("no ", date.of.interest, " in the dataset"))
    } else {
        print(paste0("Found ", date.of.interest, 
                     " in the dataset, proceed to check antecedent water deficit severity"))
    }
    

    ### read in the R database
    myData <- readRDS(paste0(sourceDir, "/pd_", user.region.name, "_regions.rds"))
    
    droughtData <- readRDS(paste0(destDir, "/antecedent_water_deficit_percentile_", 
                                  duration, "_",
                                  user.region.name, "_regions.rds"))

    
    ### dimension information
    dim1 <- dim(myData)[1]
    dim2 <- dim(myData)[2]

    ### prepare storage DF to store extreme drought and storm information
    drought.on.date.of.interest <- array(NA, c(dim1, 1))

    drought.severity.on.date.of.interest <- array(NA, c(dim1, 1))

    ### loop each grid
    for (i in c(1:dim1)) {

            ### calculate drought index based on pre-defined date of interest
            ### get precipitation percentile
           if (duration == "1-year") {
                
                ### get the rainfall data for each grid
                all <- myData[i,]
                
                ### assign NA to first two days
                DF2 <- c()
                DF2[1:11] <- NA
                
                ## calculate 1-year running total
                for (k in c(12:dim2)) {
                    DF2[k] <- sum(all[(k-11):k], na.rm=T)
                }
                
                ## obtain 1-year P - PET before the date of interest
                total.diff <- DF2[n.index]
                
            } else if (duration == "2-year") {
                
                ### get the rainfall data for each grid
                all <- myData[i,]
                
                ### assign NA to first two days
                DF2 <- c()
                DF2[1:23] <- NA
                
                ## calculate 1-year running total
                for (k in c(24:dim2)) {
                    DF2[k] <- sum(all[(k-23):k], na.rm=T)
                }
                
                ## obtain 2-year P - PET before the date of interest
                total.diff <- DF2[n.index]
                
            } else {
                print("no calculation option")
            }
            
            ### assign value
            drought.on.date.of.interest[i] <- total.diff
            
            
            ### get drought extreme information
            drought.P999 <- droughtData[i,1]
            drought.P99 <- droughtData[i,2]
            drought.P95 <- droughtData[i,3]
            drought.P90 <- droughtData[i,4]
            drought.P80 <- droughtData[i,5]
            drought.P70 <- droughtData[i,6]
            drought.P60 <- droughtData[i,7]
            drought.P50 <- droughtData[i,8]
            drought.P40 <- droughtData[i,9]
            
            ### checking drought severity
            if (drought.on.date.of.interest[i]<=drought.P999) {
                
                drought.severity.on.date.of.interest[i] <- 99.9
                
            } else if (drought.on.date.of.interest[i]<=drought.P99 & drought.on.date.of.interest[i]>drought.P999) {
                
                drought.severity.on.date.of.interest[i] <- 99.0
                
            } else if (drought.on.date.of.interest[i]<=drought.P95 & drought.on.date.of.interest[i]>drought.P99) {
                
                drought.severity.on.date.of.interest[i] <- 95.0
                
            } else if (drought.on.date.of.interest[i]<=drought.P90 & drought.on.date.of.interest[i]>drought.P95) {
                
                drought.severity.on.date.of.interest[i] <- 90.0
                
            } else if (drought.on.date.of.interest[i]<=drought.P80 & drought.on.date.of.interest[i]>drought.P90) {
                
                drought.severity.on.date.of.interest[i] <- 80.0
                
            } else if (drought.on.date.of.interest[i]<=drought.P70 & drought.on.date.of.interest[i]>drought.P80) {
                
                drought.severity.on.date.of.interest[i] <- 70.0
                
            } else if (drought.on.date.of.interest[i]<=drought.P60 & drought.on.date.of.interest[i]>drought.P70) {
                
                drought.severity.on.date.of.interest[i] <- 60.0
                
            } else if (drought.on.date.of.interest[i]<=drought.P50 & drought.on.date.of.interest[i]>drought.P60) {
                
                drought.severity.on.date.of.interest[i] <- 50.0
                
            } else {
                drought.severity.on.date.of.interest[i] <- 40.0
            }
            
            
    } # i loop
    
    ### expand the array to add lat and lon information
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
    
    latlonDF.sub$severity <- drought.severity.on.date.of.interest
    latlonDF.sub$intensity <- drought.on.date.of.interest
    
    
    ### save output
    saveRDS(latlonDF.sub, 
            file=paste0(destDir, "/antecedent_water_deficit_severity_and_intensity_", 
                        date.of.interest, "_",
                        duration, "_",
                        user.region.name, "_regions.rds"))
    
} 
