calculate_VPD_based_on_es_and_vp3pm <- function (sourceDir,
                                                 destDir,
                                                 varName,
                                                 user.region.name) {
    
    ### read in the R database
    es <- readRDS(paste0(sourceDir, "/es_", user.region.name, "_regions.rds"))
    vp3pm <- readRDS(paste0(sourceDir, "/vp3pm_", user.region.name, "_regions.rds"))
    
    ### matrix deletion
    ### following Monteith and Unsworth (1990):
    ### D (kPa) = es - ea
    out <- es - vp3pm
    
    ### save output
    saveRDS(out, file=paste0(destDir, "/", varName, "_", user.region.name, "_regions.rds"))
    
}