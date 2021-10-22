unzip_all_z_files_and_rename <- function(sourceDir) {
    
    
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
        
        ### to file
        to.text <- paste0("rain_", substr(myfiles[j], 1, 8), ".grid")
        
        to.file.command <- paste0(destDir, to.text)
        
        ### unzip files phrase
        unzip.command <- paste0("uncompress ", 
                                from.file.command, " ", 
                                destDir)
        
        ### system command
        system(unzip.command)
    }
    
    
    ### rename
    new.date.list <- paste0(day.list, day.list)
    
    
    
    for (j in 1:length(new.date.list)) {
        ### from file
        from.file.command <- paste0(sourceDir, new.date.list[j], ".grid")
        
        ### to file
        to.text <- paste0("rain_", substr(new.date.list[j], 1, 8), ".grid")
        
        to.file.command <- paste0(destDir, to.text)
        
        ### unzip files phrase
        unzip.command <- paste0("mv ", 
                                from.file.command, " ", 
                                to.file.command)
        
        ### system command
        system(unzip.command)
    }
    
}
