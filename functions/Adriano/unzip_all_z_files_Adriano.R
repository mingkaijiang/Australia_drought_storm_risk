unzip_all_z_files_Adriano <- function(sourceDir) {
    
    ### prepare date list
    day.list <- seq.Date(as.Date("2020/04/01"), 
                         as.Date("2020/11/30"), 
                         by="day")
    
    day.list <- gsub("-", "", day.list)
    
    ### prepare outdir
    destDir <- sourceDir
    
    ### get all the files with z
    myfiles <- list.files(path=sourceDir, pattern=".Z")
    
    for (j in 1:length(myfiles)) {
        ### from file
        from.file.command <- paste0(sourceDir, myfiles[j])
        
        ### unzip files phrase
        unzip.command <- paste0("uncompress ", 
                                from.file.command, " ", 
                                destDir)
        
        ### system command
        system(unzip.command)
    }
    
    
}