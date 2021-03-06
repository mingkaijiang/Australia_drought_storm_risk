compute_storm_index_for_user_defined_regions <- function(sourceDir, destDir, 
                                                         user.region.name, duration) {
    
    #### Create output folder
    if(!dir.exists(destDir)) {
        dir.create(destDir, showWarnings = FALSE)
    }
    
    ### read in the R database
    myData <- readRDS(paste0(sourceDir, "/rain_", user.region.name, "_regions.rds"))
    
    ### dimension information
    dim1 <- dim(myData)[1]
    dim2 <- dim(myData)[2]
    dim3 <- dim(myData)[3]
    
    
    ### prepare storage DF to store extreme rainfall percentile information
    out_percentile <- array(NA, c(dim1, dim2, 9))
    
    
    ### loop each grid
    for (i in 1:dim1) {
        for (j in 1:dim2) {
            ### get the rainfall data for each grid
            all <- myData[i,j,]

            ### get precipitation percentile
            if (duration == "1-day") {
                
                ## remove zero precipitation
                sub <- all[!all==0]
                
                ## percentile
                P999 <- quantile(sub, 0.999, na.rm=T)
                P99 <- quantile(sub, 0.99, na.rm=T)
                P95 <- quantile(sub, 0.95, na.rm=T)
                P90 <- quantile(sub, 0.90, na.rm=T)
                P80 <- quantile(sub, 0.80, na.rm=T)
                P70 <- quantile(sub, 0.70, na.rm=T)
                P60 <- quantile(sub, 0.60, na.rm=T)
                P50 <- quantile(sub, 0.50, na.rm=T)
                P40 <- quantile(sub, 0.40, na.rm=T)
                
                
            } else if (duration == "2-day") {
                
                ### assign NA to first day
                DF2 <- c()
                DF2[1] <- NA
                
                ## calculate 2-day running total
                for (k in c(2:dim3)) {
                    DF2[k] <- sum(all[(k-1):k])
                }
                
                sub <- DF2[DF2>0.0]
                
                ## percentile
                P999 <- quantile(sub, 0.999, na.rm=T)
                P99 <- quantile(sub, 0.99, na.rm=T)
                P95 <- quantile(sub, 0.95, na.rm=T)
                P90 <- quantile(sub, 0.90, na.rm=T)
                P80 <- quantile(sub, 0.80, na.rm=T)
                P70 <- quantile(sub, 0.70, na.rm=T)
                P60 <- quantile(sub, 0.60, na.rm=T)
                P50 <- quantile(sub, 0.50, na.rm=T)
                P40 <- quantile(sub, 0.40, na.rm=T)
                
            } else if (duration == "3-day") {
                
                ### assign NA to first two days
                DF2 <- c()
                DF2[1] <- NA
                DF2[2] <- NA
                
                ## calculate 3-day running total
                for (k in c(3:dim3)) {
                    DF2[k] <- sum(all[(k-2):k])
                }
                
                sub <- DF2[DF2>0.0]
                
                ## percentile
                P999 <- quantile(sub, 0.999, na.rm=T)
                P99 <- quantile(sub, 0.99, na.rm=T)
                P95 <- quantile(sub, 0.95, na.rm=T)
                P90 <- quantile(sub, 0.90, na.rm=T)
                P80 <- quantile(sub, 0.80, na.rm=T)
                P70 <- quantile(sub, 0.70, na.rm=T)
                P60 <- quantile(sub, 0.60, na.rm=T)
                P50 <- quantile(sub, 0.50, na.rm=T)
                P40 <- quantile(sub, 0.40, na.rm=T)
                
            } else if (duration == "4-day") {
                
                ### assign NA to first three days
                DF2 <- c()
                DF2[1] <- NA
                DF2[2] <- NA
                DF2[3] <- NA
                
                ## calculate 4-day running total
                for (k in c(4:dim3)) {
                    DF2[k] <- sum(all[(k-3):k])
                }
                
                sub <- DF2[DF2>0.0]
                
                ## percentile
                P999 <- quantile(sub, 0.999, na.rm=T)
                P99 <- quantile(sub, 0.99, na.rm=T)
                P95 <- quantile(sub, 0.95, na.rm=T)
                P90 <- quantile(sub, 0.90, na.rm=T)
                P80 <- quantile(sub, 0.80, na.rm=T)
                P70 <- quantile(sub, 0.70, na.rm=T)
                P60 <- quantile(sub, 0.60, na.rm=T)
                P50 <- quantile(sub, 0.50, na.rm=T)
                P40 <- quantile(sub, 0.40, na.rm=T)
                
            } else if (duration == "5-day") {
                
                ### assign NA to first four days
                DF2 <- c()
                DF2[1] <- NA
                DF2[2] <- NA
                DF2[3] <- NA
                DF2[4] <- NA
                
                ## calculate 5-day running total
                for (k in c(5:dim3)) {
                    DF2[k] <- sum(all[(k-4):k])
                }
                
                sub <- DF2[DF2>0.0]
                
                ## percentile
                P999 <- quantile(sub, 0.999, na.rm=T)
                P99 <- quantile(sub, 0.99, na.rm=T)
                P95 <- quantile(sub, 0.95, na.rm=T)
                P90 <- quantile(sub, 0.90, na.rm=T)
                P80 <- quantile(sub, 0.80, na.rm=T)
                P70 <- quantile(sub, 0.70, na.rm=T)
                P60 <- quantile(sub, 0.60, na.rm=T)
                P50 <- quantile(sub, 0.50, na.rm=T)
                P40 <- quantile(sub, 0.40, na.rm=T)
                
            } else {
                print("no calculation option")
            }
            
            ### assign value
            out_percentile[i,j, 1] <- P999
            out_percentile[i,j, 2] <- P99
            out_percentile[i,j, 3] <- P95
            out_percentile[i,j, 4] <- P90
            out_percentile[i,j, 5] <- P80
            out_percentile[i,j, 6] <- P70
            out_percentile[i,j, 7] <- P60
            out_percentile[i,j, 8] <- P50
            out_percentile[i,j, 9] <- P40
            
        } # j loop
    } # i loop
    

    ### write output    
    saveRDS(out_percentile, file=paste0(destDir, "/Storm_extreme_percentile_", duration,
                                        "_", user.region.name, "_regions.rds"))
    
}  