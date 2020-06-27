compute_drought_and_storm_event_severity_for_user_defined_regions <- function(sourceDir, 
                                                                              destDir, 
                                                                              user.region.name,
                                                                              date.of.interest,
                                                                              storm.duration) {
    
    
    ### translate date of interest into nday information
    date.list <- seq.Date(as.Date("1900/01/01"), 
                          as.Date("2020/03/31"), 
                          by="day")
    date.list <- gsub("-", "", date.list)
    nday <- match(date.of.interest, date.list)
    
    ### print statement
    if (is.na(nday)) {
        stop(paste0("no ", date.of.interest, " in the dataset"))
    } else {
        print(paste0("Found ", date.of.interest, 
                     " in the dataset, proceed to check storm event severity"))
    }
    

    ### read in the R database
    myData <- readRDS(paste0(sourceDir, "/rain_", user.region.name, "_regions.rds"))
    
    stormData <- readRDS(paste0(destDir, "/Storm_extreme_percentile_", storm.duration, "_",
                                user.region.name, "_regions.rds"))

    returnData <- readRDS(paste0(destDir, "/Storm_extreme_return_time_", storm.duration, "_",
                                 user.region.name, "_regions.rds"))
    
    ### dimension information
    dim1 <- dim(myData)[1]
    dim2 <- dim(myData)[2]
    dim3 <- dim(myData)[3]
    
    ### prepare storage DF to store extreme drought and storm information
    storm.on.date.of.interest <- array(NA, c(dim1, dim2))
    
    storm.severity.on.date.of.interest <- array(NA, c(dim1, dim2))
    
    storm.return.severity.on.date.of.interest <- array(NA, c(dim1, dim2))
    
    ### loop each grid
    for (i in c(1:dim1)) {
        for (j in c(1:dim2)) {

            ### calculate storm on date of interest, based on pre-defined duration threshold
            ### get storm intensity based on duration threshold
            if (storm.duration == "1-day") {
                
                ### get the rainfall data for each grid
                rainfall.on.date.of.interest <- myData[i,j,nday]
                
            } else if (storm.duration == "5-day") {
                
                ### get the rainfall data for each grid
                rainfall.on.date.of.interest <- sum(c(myData[i,j,nday], 
                                                      myData[i,j,nday+1],
                                                      myData[i,j,nday+2],
                                                      myData[i,j,nday+3],
                                                      myData[i,j,nday+4]), na.rm=T)
                
            } else {
                print("no calculation option")
            }
            
            storm.on.date.of.interest[i,j] <- rainfall.on.date.of.interest
            
            
            ### we now have the short-term rainfall intensity and long-term rainfall total information,
            ### we can compare it against the index data.
            
            ### get storm extreme index information
            storm.P999 <- ifelse(is.na(stormData[i,j,1]), 0, stormData[i,j,1])
            storm.P99 <- ifelse(is.na(stormData[i,j,2]), 0, stormData[i,j,2])
            storm.P95 <- ifelse(is.na(stormData[i,j,3]), 0, stormData[i,j,3])
            storm.P90 <- ifelse(is.na(stormData[i,j,4]), 0, stormData[i,j,4])
            storm.P80 <- ifelse(is.na(stormData[i,j,5]), 0, stormData[i,j,5])
            storm.P70 <- ifelse(is.na(stormData[i,j,6]), 0, stormData[i,j,6])
            storm.P60 <- ifelse(is.na(stormData[i,j,7]), 0, stormData[i,j,7])
            storm.P50 <- ifelse(is.na(stormData[i,j,8]), 0, stormData[i,j,8])
            storm.P40 <- ifelse(is.na(stormData[i,j,9]), 0, stormData[i,j,9])
            
            ### get storm extreme index information
            return.P99 <- ifelse(is.na(returnData[i,j,1]), 0, returnData[i,j,1])
            return.P98 <- ifelse(is.na(returnData[i,j,2]), 0, returnData[i,j,2])
            return.P95 <- ifelse(is.na(returnData[i,j,3]), 0, returnData[i,j,3])
            return.P90 <- ifelse(is.na(returnData[i,j,4]), 0, returnData[i,j,4])
            return.P80 <- ifelse(is.na(returnData[i,j,5]), 0, returnData[i,j,5])
            return.P70 <- ifelse(is.na(returnData[i,j,6]), 0, returnData[i,j,6])
            return.P60 <- ifelse(is.na(returnData[i,j,7]), 0, returnData[i,j,7])
            return.P50 <- ifelse(is.na(returnData[i,j,8]), 0, returnData[i,j,8])
            return.P40 <- ifelse(is.na(returnData[i,j,9]), 0, returnData[i,j,9])
            
            
            ### check storm severity
            if (storm.on.date.of.interest[i,j]>=storm.P999) {
                
                storm.severity.on.date.of.interest[i,j] <- 99.9
                
            } else if (storm.on.date.of.interest[i,j]>=storm.P99 & storm.on.date.of.interest[i,j]<storm.P999) {
                
                storm.severity.on.date.of.interest[i,j] <- 99.0
                
            } else if (storm.on.date.of.interest[i,j]>=storm.P95 & storm.on.date.of.interest[i,j]<storm.P99) {
                
                storm.severity.on.date.of.interest[i,j] <- 95.0
                
            } else if (storm.on.date.of.interest[i,j]>=storm.P90 & storm.on.date.of.interest[i,j]<storm.P95) {
                
                storm.severity.on.date.of.interest[i,j] <- 90.0
                
            } else if (storm.on.date.of.interest[i,j]>=storm.P80 & storm.on.date.of.interest[i,j]<storm.P90) {
                
                storm.severity.on.date.of.interest[i,j] <- 80.0
                
            } else if (storm.on.date.of.interest[i,j]>=storm.P70 & storm.on.date.of.interest[i,j]<storm.P80) {
                
                storm.severity.on.date.of.interest[i,j] <- 70.0
                
            } else if (storm.on.date.of.interest[i,j]>=storm.P60 & storm.on.date.of.interest[i,j]<storm.P70) {
                
                storm.severity.on.date.of.interest[i,j] <- 60.0
                
            } else if (storm.on.date.of.interest[i,j]>=storm.P50 & storm.on.date.of.interest[i,j]<storm.P60) {
                
                storm.severity.on.date.of.interest[i,j] <- 50.0
                
            } else {
                storm.severity.on.date.of.interest[i,j] <- 40.0
                
            }
            
            ### check storm severity based on return interval
            if (storm.on.date.of.interest[i,j]>=return.P99) {
                
                storm.return.severity.on.date.of.interest[i,j] <- 100
                
            } else if (storm.on.date.of.interest[i,j]>=return.P98 & storm.on.date.of.interest[i,j]<return.P99) {
                
                storm.return.severity.on.date.of.interest[i,j] <- 50.0
                
            } else if (storm.on.date.of.interest[i,j]>=return.P95 & storm.on.date.of.interest[i,j]<return.P98) {
                
                storm.return.severity.on.date.of.interest[i,j] <- 20.0
                
            } else if (storm.on.date.of.interest[i,j]>=return.P90 & storm.on.date.of.interest[i,j]<return.P95) {
                
                storm.return.severity.on.date.of.interest[i,j] <- 10.0
                
            } else if (storm.on.date.of.interest[i,j]>=return.P80 & storm.on.date.of.interest[i,j]<return.P90) {
                
                storm.return.severity.on.date.of.interest[i,j] <- 5.0
                
            } else if (storm.on.date.of.interest[i,j]>=return.P70 & storm.on.date.of.interest[i,j]<return.P80) {
                
                storm.return.severity.on.date.of.interest[i,j] <- 3.3
                
            } else if (storm.on.date.of.interest[i,j]>=return.P60 & storm.on.date.of.interest[i,j]<return.P70) {
                
                storm.return.severity.on.date.of.interest[i,j] <- 2.5
                
            } else if (storm.on.date.of.interest[i,j]>=return.P50 & storm.on.date.of.interest[i,j]<return.P60) {
                
                storm.return.severity.on.date.of.interest[i,j] <- 2.0
                
            } else {
                storm.return.severity.on.date.of.interest[i,j] <- 1.67
                
            }
            
        } # j loop
    } # i loop
    
    
    ### save output
    saveRDS(storm.severity.on.date.of.interest, 
            file=paste0(destDir, "/storm_severity_", date.of.interest, "_",
                        storm.duration, "_",
                        user.region.name, "_regions.rds"))
    
    saveRDS(storm.return.severity.on.date.of.interest, 
            file=paste0(destDir, "/storm_return_severity_", date.of.interest, "_",
                        storm.duration, "_",
                        user.region.name, "_regions.rds"))
    
    saveRDS(storm.on.date.of.interest, 
            file=paste0(destDir, "/storm_intensity_", date.of.interest, "_",
                        storm.duration, "_",
                        user.region.name, "_regions.rds"))
    
    
} 