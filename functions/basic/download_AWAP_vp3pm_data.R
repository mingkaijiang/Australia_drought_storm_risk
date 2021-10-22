
download_AWAP_vp3pm_data <- function(destDir) {
    
    ### remote url
    url1 <- "http://www.bom.gov.au/web03/ncc/www/awap/vprp/vprph15/daily/grid/0.05/history/nat/"
    
    ### file names
    day.list <- seq.Date(as.Date("2020/04/01"), 
                         as.Date("2020/11/30"), 
                         by="day")
    
    day.list <- gsub("-", "", day.list)
    
    
    ### create storage directories
    yr.list <- c(1971:2020)
    
    for (i in yr.list) {
        if(!dir.exists(paste0(destDir, i, "/"))) {
            dir.create(paste0(destDir, i, "/"), showWarnings = FALSE)
        }
    }


    
    ### download command
    for (i in day.list) {
        
        subDir <- substr(i, 1, 4)
            
        download.file(url=paste0(url1, i, i, ".grid.Z"),
                      destfile=paste0(destDir, subDir, "/", i, i, ".grid.Z"))
        
    }
    
}

