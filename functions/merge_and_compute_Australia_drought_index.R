merge_and_compute_Australia_drought_index <- function(sourceDir, destDir,
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
                if (duration == "no.rain.period") {
                    
                    ## check data distribution
                    test1 <- rle(all)
                    
                    ## convert into dataframe
                    DF1 <- data.frame(test1$lengths, test1$values)
                    colnames(DF1) <- c("lengths", "values")
                    DF1$cumsum <- cumsum(DF1$lengths)
                    DF1$loc1 <- c(1:dim(DF1)[1])
                    
                    ## subset no rain DF
                    sub <- subset(DF1, values == 0)
                    
                    ## obtain statistics
                    max.no.rain.duration <- round(max(sub$lengths, na.rm=T), 0)
                    P999 <- round(quantile(sub$lengths, 0.999, na.rm=T),0)
                    P99 <- round(quantile(sub$lengths, 0.99, na.rm=T),0)
                    P95 <- round(quantile(sub$lengths, 0.95, na.rm=T),0)
                    P90 <- round(quantile(sub$lengths, 0.90, na.rm=T),0)

                    ### assign value
                    out_percentile[i,j, 1] <- max.no.rain.duration
                    out_percentile[i,j, 2] <- P999
                    out_percentile[i,j, 3] <- P99
                    out_percentile[i,j, 4] <- P95
                    out_percentile[i,j, 5] <- P90

                } else if (duration == "1-year") {
                    
                    ### assign NA to first two days
                    DF2 <- c()
                    DF2[1:365] <- NA
                    
                    ## calculate 1-year running total
                    for (k in c(366:dim3)) {
                        DF2[k] <- sum(all[(k-365):k], na.rm=T)
                        k <- k+1
                    }
                    
                    
                    ## obtain statistics
                    P999 <- quantile(DF2, 0.001, na.rm=T)
                    P99 <- quantile(DF2, 0.01, na.rm=T)
                    P95 <- quantile(DF2, 0.05, na.rm=T)
                    P90 <- quantile(DF2, 0.1, na.rm=T)
                    P80 <- quantile(DF2, 0.2, na.rm=T)
                    
                    ### assign value
                    out_percentile[i,j, 1] <- P999
                    out_percentile[i,j, 2] <- P99
                    out_percentile[i,j, 3] <- P95
                    out_percentile[i,j, 4] <- P90
                    out_percentile[i,j, 5] <- P80
                    
                } else if (duration == "2-year") {
                    
                    ### assign NA to first two days
                    DF2 <- c()
                    DF2[1:730] <- NA
                    
                    ## calculate 1-year running total
                    for (k in c(731:dim3)) {
                        DF2[k] <- sum(all[(k-730):k], na.rm=T)
                        k <- k+1
                    }
                    
                    
                    ## obtain statistics
                    P999 <- quantile(DF2, 0.001, na.rm=T)
                    P99 <- quantile(DF2, 0.01, na.rm=T)
                    P95 <- quantile(DF2, 0.05, na.rm=T)
                    P90 <- quantile(DF2, 0.1, na.rm=T)
                    P80 <- quantile(DF2, 0.2, na.rm=T)
                    
                    ### assign value
                    out_percentile[i,j, 1] <- P999
                    out_percentile[i,j, 2] <- P99
                    out_percentile[i,j, 3] <- P95
                    out_percentile[i,j, 4] <- P90
                    out_percentile[i,j, 5] <- P80
                    
                } else {
                    print("no calculation option")
                }
                
            } # j loop
        } # i loop
        
        ### write output    
        saveRDS(out_percentile, file=paste0(destDir, "/Group_", file.id, 
                                            "_Drought_extreme_percentile_", duration,
                                            "_Australia.rds"))
        
        ### unlist several large files to save space
        rm(myData)
        rm(out_percentile)
        
        
    } # file.id
    

}  