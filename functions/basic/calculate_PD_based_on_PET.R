calculate_PD_based_on_PET <- function (sourceDir,
                                       destDir,
                                       varName,
                                       user.lat.max,
                                       user.lat.min,
                                       user.lon.max,
                                       user.lon.min,
                                       user.region.name) {
    
    
    sourceDir = "input"
    destDir = "input"
    varName = "pd"
    user.lat.max = -31
    user.lat.min = -35
    user.lon.max = 153
    user.lon.min = 149
    user.region.name = "SydneyHunter"
    
    
    ### read in the PET monthly dataframe
    pet <- readRDS(paste0(sourceDir, "/pet_", user.region.name, "_regions.rds"))
    
    ### read in rain daily
    rain <- readRDS(paste0(sourceDir, "/rain_", user.region.name, "_regions.rds"))
    
    
    print("nothing written yet")
    
    
    
    
    ### save output
    saveRDS(out, file=paste0(destDir, "/", varName, "_", user.region.name, "_regions.rds"))
    
}