calculate_PD_based_on_PET <- function (sourceDir,
                                         destDir,
                                         varName,
                                         user.region.name) {
    
    ### read in the R database
    pet <- readRDS(paste0(sourceDir, "/pet_", user.region.name, "_regions.rds"))
    rain <- readRDS(paste0(sourceDir, "/rain_", user.region.name, "_regions.rds"))
    
    
    print("nothing written yet")
    
    
    
    
    ### save output
    saveRDS(out, file=paste0(destDir, "/", varName, "_", user.region.name, "_regions.rds"))
    
}