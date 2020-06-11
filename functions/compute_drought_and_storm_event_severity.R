compute_drought_and_storm_event_severity <- function(sourceDir, 
                                                     destDir, 
                                                     inFile,
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
    #myData <- readRDS(paste0(sourceDir, "/", inFile))
    myData <- readRDS("input/Group_1.rds")
    stormData <- readRDS(paste0(destDir, "/Group_1_1-day_storm_extreme_percentile.rds"))
    droughtData <- readRDS(paste0(destDir, "/Group_1_1-year_drought_extreme_percentile.rds"))
    
    ### dimension information
    dim1 <- dim(myData)[1]
    dim2 <- dim(myData)[2]
    dim3 <- dim(myData)[3]
    
    ### prepare storage DF to store extreme drought and storm information
    drought.on.date.of.interest <- array(NA, c(dim1, dim2))
    storm.on.date.of.interest <- array(NA, c(dim1, dim2))
    
    ### calculate storm on date of interest, based on pre-defined duration threshold
    ### loop each grid
    for (i in 1:dim1) {
        for (j in 1:dim2) {

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
            
        } # j loop
    } # i loop
    
    
    ### calculate drought index based on pre-defined date of interest
    ### loop each grid
    for (i in 1:dim1) {
        for (j in 1:dim2) {
            
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
                    k <- k+1
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
                    k <- k+1
                }
                
                ## obtain 1-year rainfall before the date of interest
                total.rainfall <- DF2[nday]
                
            } else {
                print("no calculation option")
            }
            
            ### assign value
            drought.on.date.of.interest[i,j] <- total.rainfall
            
        } # j loop
    } # i loop
    
    ### we now have the short-term rainfall intensity and long-term rainfall total information,
    ### we can compare it against the index data.
    
    
    
    ### save output
    saveRDS(out_percentile, file=paste0(destDir, 
                                        "/Sydney_regions_", storm.duration, "_",
                                        drought.duration,
                                        "_storm_extreme_percentile.rds"))
    
} 