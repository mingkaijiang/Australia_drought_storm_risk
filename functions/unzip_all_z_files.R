unzip_all_z_files <- function(sourceDir, s.yr, e.yr) {
    
    tseries <- seq(s.yr, e.yr, by=1)
    
    destDir <- sourceDir
        
    for (j in tseries) {
        ### unzip files phrase
        unzip.command <- paste0("uncompress ", 
                                sourceDir, j, "/* ", 
                                destDir, "", j)
        
        ### system command
        system(unzip.command)
    }
    
    
}