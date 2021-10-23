calculate_saturated_vapor_pressure_based_on_Tmax_Adriano <- function (sourceDir,
                                                                      destDir,
                                                                      varName,
                                                                      user.region.name) {
    
    
    ### read in the R database
    myData <- readRDS(paste0(sourceDir, "/tmax_", user.region.name, "_sites.rds"))
    
    ### equation for es:
    ### es = 0.6108 * exp(17.27 * (Tair/(Tair+237.3)))
    
    out <- 0.6108 * exp(17.27 * (myData/(myData+237.3)))
    
    ### save output
    saveRDS(out, file=paste0(destDir, "/", varName, "_", user.region.name, "_sites.rds"))
    
}