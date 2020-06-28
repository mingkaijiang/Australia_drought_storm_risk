compute_antecedent_atmospheric_dryness_severity_for_user_defined_regions <- function(sourceDir, 
                                                                                    destDir, 
                                                                                    user.region.name,
                                                                                    date.of.interest,
                                                                                    duration) {
    
    
    ### translate date of interest into nday information
    date.list <- seq.Date(as.Date("1971/01/01"), 
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
    myData <- readRDS(paste0(sourceDir, "/vpd_", user.region.name, "_regions.rds"))
    
    droughtData <- readRDS(paste0(destDir, "/antecedent_atmospheric_dryness_percentile_", 
                                  duration, "_",
                                  user.region.name, "_regions.rds"))

    
    ### dimension information
    dim1 <- dim(myData)[1]
    dim2 <- dim(myData)[2]
    dim3 <- dim(myData)[3]
    
    ### prepare storage DF to store extreme drought and storm information
    drought.on.date.of.interest <- array(NA, c(dim1, dim2))

    drought.severity.on.date.of.interest <- array(NA, c(dim1, dim2))

    ### loop each grid
    for (i in c(1:dim1)) {
        for (j in c(1:dim2)) {

            ### calculate drought index based on pre-defined date of interest
            ### get precipitation percentile
           if (duration == "1-year") {
                
                ### get the rainfall data for each grid
                all <- myData[i,j,]
                
                ### assign NA to first two days
                DF2 <- c()
                DF2[1:365] <- NA
                
                ## calculate 1-year running total
                for (k in c(366:dim3)) {
                    DF2[k] <- mean(all[(k-365):k], na.rm=T)
                }
                
                ## obtain 1-year rainfall before the date of interest
                total.rainfall <- DF2[nday]
                
            } else if (duration == "2-year") {
                
                ### get the rainfall data for each grid
                all <- myData[i,j,]
                
                ### assign NA to first two days
                DF2 <- c()
                DF2[1:730] <- NA
                
                ## calculate 1-year running total
                for (k in c(731:dim3)) {
                    DF2[k] <- mean(all[(k-730):k], na.rm=T)
                }
                
                ## obtain 1-year rainfall before the date of interest
                total.rainfall <- DF2[nday]
                
            } else {
                print("no calculation option")
            }
            
            ### assign value
            drought.on.date.of.interest[i,j] <- total.rainfall
            
            
            ### get drought extreme information
            drought.P999 <- droughtData[i,j,1]
            drought.P99 <- droughtData[i,j,2]
            drought.P95 <- droughtData[i,j,3]
            drought.P90 <- droughtData[i,j,4]
            drought.P80 <- droughtData[i,j,5]
            drought.P70 <- droughtData[i,j,6]
            drought.P60 <- droughtData[i,j,7]
            drought.P50 <- droughtData[i,j,8]
            drought.P40 <- droughtData[i,j,9]
            
            ### checking drought severity
            if (drought.on.date.of.interest[i,j]<=drought.P40) {
                
                drought.severity.on.date.of.interest[i,j] <- 40.0
                
            } else if (drought.on.date.of.interest[i,j]<=drought.P50 & drought.on.date.of.interest[i,j]>drought.P40) {
                
                drought.severity.on.date.of.interest[i,j] <- 50.0
                
            } else if (drought.on.date.of.interest[i,j]<=drought.P60 & drought.on.date.of.interest[i,j]>drought.P50) {
                
                drought.severity.on.date.of.interest[i,j] <- 60.0
                
            } else if (drought.on.date.of.interest[i,j]<=drought.P70 & drought.on.date.of.interest[i,j]>drought.P60) {
                
                drought.severity.on.date.of.interest[i,j] <- 70.0
                
            } else if (drought.on.date.of.interest[i,j]<=drought.P80 & drought.on.date.of.interest[i,j]>drought.P70) {
                
                drought.severity.on.date.of.interest[i,j] <- 80.0
                
            } else if (drought.on.date.of.interest[i,j]<=drought.P90 & drought.on.date.of.interest[i,j]>drought.P80) {
                
                drought.severity.on.date.of.interest[i,j] <- 90.0
                
            } else if (drought.on.date.of.interest[i,j]<=drought.P95 & drought.on.date.of.interest[i,j]>drought.P90) {
                
                drought.severity.on.date.of.interest[i,j] <- 95.0
                
            } else if (drought.on.date.of.interest[i,j]<=drought.P99 & drought.on.date.of.interest[i,j]>drought.P95) {
                
                drought.severity.on.date.of.interest[i,j] <- 99.0
                
            } else {
                drought.severity.on.date.of.interest[i,j] <- 99.9
            }
            
            
        } # j loop
    } # i loop
    
    
    ### save output
    saveRDS(drought.severity.on.date.of.interest, 
            file=paste0(destDir, "/antecedent_atmospheric_dryness_severity_", date.of.interest, "_",
                        duration, "_",
                        user.region.name, "_regions.rds"))
    
    saveRDS(drought.on.date.of.interest, 
            file=paste0(destDir, "/antecedent_atmospheric_dryness_intensity_", date.of.interest, "_",
                        duration, "_",
                        user.region.name, "_regions.rds"))
    
} 