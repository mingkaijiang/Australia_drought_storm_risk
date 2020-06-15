compute_drought_and_storm_event_severity_for_user_defined_regions <- function(sourceDir, 
                                                                              destDir, 
                                                                              user.region.name,
                                                                              date.of.interest,
                                                                              storm.duration,
                                                                              drought.duration) {
    
    
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
    myData <- readRDS(paste0(sourceDir, "/", user.region.name, "_regions.rds"))
    stormData <- readRDS(paste0(destDir, "/Storm_extreme_percentile_", storm.duration, "_",
                                user.region.name, "_regions.rds"))
    droughtData <- readRDS(paste0(destDir, "/Drought_extreme_percentile_", drought.duration, "_",
                                  user.region.name, "_regions.rds"))
    
    ### dimension information
    dim1 <- dim(myData)[1]
    dim2 <- dim(myData)[2]
    dim3 <- dim(myData)[3]
    
    ### prepare storage DF to store extreme drought and storm information
    drought.on.date.of.interest <- array(NA, c(dim1, dim2))
    storm.on.date.of.interest <- array(NA, c(dim1, dim2))
    
    drought.severity.on.date.of.interest <- array(NA, c(dim1, dim2))
    storm.severity.on.date.of.interest <- array(NA, c(dim1, dim2))
    
    ### loop each grid
    for (i in c(1:dim1)) {
        for (j in c(1:dim2)) {

            ### calculate storm on date of interest, based on pre-defined duration threshold
            ### get storm intensity based on duration threshold
            if (storm.duration == "1-day") {
                
                ### get the rainfall data for each grid
                rainfall.on.date.of.interest <- myData[i,j,nday]
                
            } else if (storm.duration == "2-day") {
                
                ### get the rainfall data for each grid
                rainfall.on.date.of.interest <- sum(c(myData[i,j,nday], 
                                                      myData[i,j,nday+1]), na.rm=T)
                
            } else if (storm.duration == "3-day") {
                
                ### get the rainfall data for each grid
                rainfall.on.date.of.interest <- sum(c(myData[i,j,nday], 
                                                      myData[i,j,nday+1],
                                                      myData[i,j,nday+2]), na.rm=T)
                
            } else if (storm.duration == "4-day") {
                
                ### get the rainfall data for each grid
                rainfall.on.date.of.interest <- sum(c(myData[i,j,nday], 
                                                      myData[i,j,nday+1],
                                                      myData[i,j,nday+2],
                                                      myData[i,j,nday+3]), na.rm=T)
                
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
            
            
            ### calculate drought index based on pre-defined date of interest
            ### get precipitation percentile
            if (drought.duration == "no.rain.period") {
                
                stop("no option for no.rain.period calculation yet")
                
            } else if (drought.duration == "1-year") {
                
                ### get the rainfall data for each grid
                all <- myData[i,j,]
                
                ### assign NA to first two days
                DF2 <- c()
                DF2[1:365] <- NA
                
                ## calculate 1-year running total
                for (k in c(366:dim3)) {
                    DF2[k] <- sum(all[(k-365):k], na.rm=T)
                }
                
                ## obtain 1-year rainfall before the date of interest
                total.rainfall <- DF2[nday]
                
            } else if (drought.duration == "2-year") {
                
                ### get the rainfall data for each grid
                all <- myData[i,j,]
                
                ### assign NA to first two days
                DF2 <- c()
                DF2[1:730] <- NA
                
                ## calculate 1-year running total
                for (k in c(731:dim3)) {
                    DF2[k] <- sum(all[(k-730):k], na.rm=T)
                }
                
                ## obtain 1-year rainfall before the date of interest
                total.rainfall <- DF2[nday]
                
            } else {
                print("no calculation option")
            }
            
            ### assign value
            drought.on.date.of.interest[i,j] <- total.rainfall
            
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
            
            ### get drought extreme information
            drought.P001 <- droughtData[i,j,1]
            drought.P01 <- droughtData[i,j,2]
            drought.P05 <- droughtData[i,j,3]
            drought.P10 <- droughtData[i,j,4]
            drought.P20 <- droughtData[i,j,5]
            drought.P30 <- droughtData[i,j,6]
            drought.P40 <- droughtData[i,j,7]
            drought.P50 <- droughtData[i,j,8]
            drought.P60 <- droughtData[i,j,9]
            
            ### checking drought severity
            if (drought.on.date.of.interest[i,j]<=drought.P001) {
                
                drought.severity.on.date.of.interest[i,j] <- 0.1
                
            } else if (drought.on.date.of.interest[i,j]<=drought.P01 & drought.on.date.of.interest[i,j]>drought.P001) {
                
                drought.severity.on.date.of.interest[i,j] <- 1.0
                
            } else if (drought.on.date.of.interest[i,j]<=drought.P05 & drought.on.date.of.interest[i,j]>drought.P01) {
                
                drought.severity.on.date.of.interest[i,j] <- 5.0
                
            } else if (drought.on.date.of.interest[i,j]<=drought.P10 & drought.on.date.of.interest[i,j]>drought.P05) {
                
                drought.severity.on.date.of.interest[i,j] <- 10.0
                
            } else if (drought.on.date.of.interest[i,j]<=drought.P20 & drought.on.date.of.interest[i,j]>drought.P10) {
                
                drought.severity.on.date.of.interest[i,j] <- 20.0
                
            } else if (drought.on.date.of.interest[i,j]<=drought.P30 & drought.on.date.of.interest[i,j]>drought.P20) {
                
                drought.severity.on.date.of.interest[i,j] <- 30.0
                
            } else if (drought.on.date.of.interest[i,j]<=drought.P40 & drought.on.date.of.interest[i,j]>drought.P30) {
                
                drought.severity.on.date.of.interest[i,j] <- 40.0
                
            } else if (drought.on.date.of.interest[i,j]<=drought.P50 & drought.on.date.of.interest[i,j]>drought.P40) {
                
                drought.severity.on.date.of.interest[i,j] <- 50.0
                
            } else {
                drought.severity.on.date.of.interest[i,j] <- 60.0
            }
            
            
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
            
        } # j loop
    } # i loop
    
    
    ### save output
    saveRDS(storm.severity.on.date.of.interest, 
            file=paste0(destDir, "/storm_severity_", date.of.interest, "_",
                        storm.duration, "_",
                        user.region.name, "_regions.rds"))
    
    saveRDS(drought.severity.on.date.of.interest, 
            file=paste0(destDir, "/drought_severity_", date.of.interest, "_",
                        drought.duration, "_",
                        user.region.name, "_regions.rds"))
    
    saveRDS(storm.on.date.of.interest, 
            file=paste0(destDir, "/storm_intensity_", date.of.interest, "_",
                        storm.duration, "_",
                        user.region.name, "_regions.rds"))
    
    saveRDS(drought.on.date.of.interest, 
            file=paste0(destDir, "/drought_intensity_", date.of.interest, "_",
                        drought.duration, "_",
                        user.region.name, "_regions.rds"))
    
} 