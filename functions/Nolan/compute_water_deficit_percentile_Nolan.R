compute_water_deficit_percentile_Nolan <- function(sourceDir, 
                                                   destDir,
                                                   duration, 
                                                   nswDF) {
    
    
    #### Create output folder
    if(!dir.exists(destDir)) {
        dir.create(destDir, showWarnings = FALSE)
    }
    
    
    ### get number of files
    n <- nrow(nswDF)
    
    ### there are n DFs available
    file.list <- c(1:n)
    
    ### read in the R database
    for (file.id in file.list) {
        myData <- readRDS(paste0(sourceDir, "/pd_NSW", file.list[file.id], "_regions.rds"))

        ### dimension information
        dim1 <- dim(myData)[1]
        dim2 <- dim(myData)[2]
        
        ### prepare storage DF to store drought percentile information
        out_percentile <- array(NA, c(dim1, 9))
        
        ### the first column stores the value, the 2nd column stores the percentile
        out2 <- array(NA, c(dim1, 2))
        
        ### period of interest is the third to last value
        p.interest <- dim2-2
        
        ### loop each grid
        for (i in 1:dim1) {
            ### get the rainfall data for each grid
            all <- myData[i,]
            
            
            ### assign NA to first two days
            DF2 <- c()
            DF2[1:23] <- NA
            
            ## calculate 2-year running total
            for (k in c(24:dim2)) {
                DF2[k] <- sum(all[(k-23):k], na.rm=T)
            }
            
            ## find value of interest
            v.interest <- DF2[p.interest]
            
            ### generate ordered array
            DF3 <- ecdf(DF2)
            
            ### find percentile information for the value of interest
            p.out <- DF3(v.interest)
            
            ### store
            out2[i, 1] <- v.interest
            out2[i, 2] <- p.out
                        
            
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
            
            
        } # i loop
        
        ### save output
        saveRDS(out_percentile, file=paste0(destDir, "/water_deficit_percentile_", duration,
                                            "_NSW", file.list[file.id], "_regions.rds"))
        
        saveRDS(out2, file=paste0(destDir, "/water_deficit_percentile_period_of_interest_NSW", 
                                  file.list[file.id], "_regions.rds"))
        
        ### clean
        rm(out_percentile)
        rm(out2)
        rm(myData)
        

    } # file.id
    


}  