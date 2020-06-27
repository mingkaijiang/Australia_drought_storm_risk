calculate_PET_based_on_Tmax <- function (sourceDir,
                                         destDir,
                                         varName,
                                         user.region.name) {
    
    ### read in the R database
    myData <- readRDS(paste0(sourceDir, "/tmax_", user.region.name, "_regions.rds"))
    
    
    print("nothing written yet")
    
    
    
    
    ### save output
    saveRDS(out, file=paste0(destDir, "/", varName, "_", user.region.name, "_regions.rds"))
    
}