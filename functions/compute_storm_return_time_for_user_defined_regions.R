compute_storm_return_time_for_user_defined_regions <- function(sourceDir, destDir, 
                                                               user.region.name, 
                                                               duration) {
    
    
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

            ### redefining duration as #days
            ### get return times, normalising by duration
                
            ### assign NA to first (duration-1) days
            DF2 <- c()
            skip <- duration-1
            DF2[1:skip] <- NA
                
            ## calculate duration-day running total
            for (k in c(duration:dim3)) {
                DF2[k] <- sum(all[(k-skip):k])
            }
                
            ## return-time
            P99 <- quantile(DF2, (1-0.01*duration/365), na.rm=T)
            P98 <- quantile(DF2, (1-0.02*duration/365), na.rm=T)
            P95 <- quantile(DF2, (1-0.05*duration/365), na.rm=T)
            P90 <- quantile(DF2, (1-0.1*duration/365), na.rm=T)
            P80 <- quantile(DF2, (1-0.2*duration/365), na.rm=T)
            P70 <- quantile(DF2, (1-0.3*duration/365), na.rm=T)
            P60 <- quantile(DF2, (1-0.4*duration/365), na.rm=T)
            P50 <- quantile(DF2, (1-0.5*duration/365), na.rm=T)
            P40 <- quantile(DF2, (1-0.6*duration/365), na.rm=T)
          
            ### assign value
            out_percentile[i,j, 1] <- P99
            out_percentile[i,j, 2] <- P98
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
    saveRDS(out_percentile, file=paste0(destDir, "/Storm_extreme_return_time_", duration,
                                        "_", user.region.name, "_regions.rds"))
    
}  
