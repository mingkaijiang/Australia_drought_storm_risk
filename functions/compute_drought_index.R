compute_drought_index <- function(sourceDir, destDir) {
    
    ### prepare year DFs
    yr.list <- c(1900:2018)
    
    n.yr <- length(yr.list)
    
    ### Prepare all year output array
    out <- array(NA, c(691, 886, 12))
    
    #### To process the raw data into format easily readable
    for (i in yr.list) {
        ### complete the path
        sDir <- paste0(sourceDir, i)

        ### Source all files in input folder
        DatFiles <- list.files(path = sDir, pattern = "\\.grid")
        
        ### Prepare output array
        if (leap_year(i)) {
            daily.tmp <- array(NA, c(691, 886, 366))
        } else {
            daily.tmp <- array(NA, c(691, 886, 365))
        }
        
        ### Read in data
        for (j in 1:length(DatFiles)) {
            inName <- file.path(sDir, DatFiles[j], fsep = .Platform$file.sep)
            myDF <- read.ascii.grid(inName)
            
            daily.tmp[, , j] <- myDF$data
        }   
        
        
        
        saveRDS(out, file=paste0(destDir, "/DF", i, ".rds"))
        
        
    }  # i loop
}   # function loop