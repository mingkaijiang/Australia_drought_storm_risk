
download_AWAP_rainfall_data <- function(destDir) {
    
    ### remote url
    url1 <- "http://www.bom.gov.au/web03/ncc/www/awap/rainfall/totals/daily/grid/0.05/history/nat/"

    ### file names
    day.list <- seq.Date(as.Date("2020/04/01"), 
                         as.Date("2020/12/31"), 
                         by="day")
    
    day.list <- gsub("-", "", day.list)

    
    ### download command
    for (i in day.list) {
        download.file(url=paste0(url1, i, i, ".grid.Z"),
                      destfile=paste0(destDir, "2020/rain_", i, ".grid.Z"))
        
    }
    
}

