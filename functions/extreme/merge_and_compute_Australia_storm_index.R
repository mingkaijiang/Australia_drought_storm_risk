merge_and_compute_Australia_storm_index <- function(sourceDir, destDir,
                                                    duration) {
    
    ### there are 23 rds available
    file.list <- c(1:23)
    
    ### read in the R database
    for (file.id in file.list) {
        myData <- readRDS(paste0(sourceDir, "/Group_", file.list[file.id], ".rds"))
        
        ### dimension information
        dim1 <- dim(myData)[1]
        dim2 <- dim(myData)[2]
        dim3 <- dim(myData)[3]
        
        ### prepare storage DF to store extreme rainfall percentile information
        out_percentile <- array(NA, c(dim1, dim2,5))
        
        
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
                    
                } else {
                    print("no calculation option")
                }
                
                ### assign value
                out_percentile[i,j, 1] <- P999
                out_percentile[i,j, 2] <- P99
                out_percentile[i,j, 3] <- P95
                out_percentile[i,j, 4] <- P90
                out_percentile[i,j, 5] <- P80
                
            } # j loop
        } # i loop
        
        ### write output    
        saveRDS(out_percentile, file=paste0(destDir, "/Group_", file.id, 
                                            "_Storm_extreme_percentile_", duration,
                                            "_Australia.rds"))
        
        ### unlist several large files to save space
        rm(myData)
        rm(out_percentile)
        
        
    } # file.id
    

}  