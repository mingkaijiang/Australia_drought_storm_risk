compute_antecedent_water_deficit_for_user_defined_regions <- function(sourceDir, 
                                                                      destDir, 
                                                                      user.region.name, 
                                                                      duration) {
    
    #### Create output folder
    if(!dir.exists(destDir)) {
        dir.create(destDir, showWarnings = FALSE)
    }
    
    ### read in the R database
    myData <- readRDS(paste0(sourceDir, "/pd_", user.region.name, "_regions.rds"))
    
    ### dimension information
    dim1 <- dim(myData)[1]
    dim2 <- dim(myData)[2]
    
    ### prepare storage DF to store drought percentile information
    out_percentile <- array(NA, c(dim1, 9))
    
    
    ### loop each grid
    for (i in 1:dim1) {
            ### get the rainfall data for each grid
            all <- myData[i,]
            
            ### get precipitation percentile
            if (duration == "1-year") {
                
                ### assign NA to first two days
                DF2 <- c()
                DF2[1:11] <- NA

                ## calculate 1-year running total
                for (k in c(12:dim2)) {
                    DF2[k] <- sum(all[(k-11):k], na.rm=T)
                }
                
                
                ## obtain statistics
                P999 <- quantile(DF2, 0.001, na.rm=T)
                P99 <- quantile(DF2, 0.01, na.rm=T)
                P95 <- quantile(DF2, 0.05, na.rm=T)
                P90 <- quantile(DF2, 0.1, na.rm=T)
                P80 <- quantile(DF2, 0.2, na.rm=T)
                P70 <- quantile(DF2, 0.3, na.rm=T)
                P60 <- quantile(DF2, 0.4, na.rm=T)
                P50 <- quantile(DF2, 0.5, na.rm=T)
                P40 <- quantile(DF2, 0.6, na.rm=T)
                
                ### assign value
                out_percentile[i,1] <- P999
                out_percentile[i,2] <- P99
                out_percentile[i,3] <- P95
                out_percentile[i,4] <- P90
                out_percentile[i,5] <- P80
                out_percentile[i,6] <- P70
                out_percentile[i,7] <- P60
                out_percentile[i,8] <- P50
                out_percentile[i,9] <- P40
                    
            } else if (duration == "2-year") {
                
                ### assign NA to first two days
                DF2 <- c()
                DF2[1:23] <- NA
                
                ## calculate 1-year running total
                for (k in c(24:dim2)) {
                    DF2[k] <- sum(all[(k-23):k], na.rm=T)
                }
                
                
                ## obtain statistics
                P999 <- quantile(DF2, 0.001, na.rm=T)
                P99 <- quantile(DF2, 0.01, na.rm=T)
                P95 <- quantile(DF2, 0.05, na.rm=T)
                P90 <- quantile(DF2, 0.1, na.rm=T)
                P80 <- quantile(DF2, 0.2, na.rm=T)
                P70 <- quantile(DF2, 0.3, na.rm=T)
                P60 <- quantile(DF2, 0.4, na.rm=T)
                P50 <- quantile(DF2, 0.5, na.rm=T)
                P40 <- quantile(DF2, 0.6, na.rm=T)
                
                ### assign value
                out_percentile[i,1] <- P999
                out_percentile[i,2] <- P99
                out_percentile[i,3] <- P95
                out_percentile[i,4] <- P90
                out_percentile[i,5] <- P80
                out_percentile[i,6] <- P70
                out_percentile[i,7] <- P60
                out_percentile[i,8] <- P50
                out_percentile[i,9] <- P40
                
            } else {
                print("no calculation option")
            }
            
            
    } # i loop
    
    saveRDS(out_percentile, file=paste0(destDir, "/antecedent_water_deficit_percentile_", duration,
                                        "_", user.region.name, "_regions.rds"))
    
    
}  