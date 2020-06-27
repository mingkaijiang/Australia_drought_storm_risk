calculate_saturated_vapor_pressure_based_on_Tmax <- function (sourceDir,
                                                              destDir,
                                                              varName,
                                                              user.region.name) {
    
    
    ### read in the R database
    myData <- readRDS(paste0(sourceDir, "/tmax_", user.region.name, "_regions.rds"))
    
    ### dimension information
    dim1 <- dim(myData)[1]
    dim2 <- dim(myData)[2]
    dim3 <- dim(myData)[3]
    
    ### equation for es:
    ### es = 0.6108 * exp(17.27 * (Tair/(Tair+237.3)))
    
    out <- 0.6108 * exp(17.27 * (myData/(myData+237.3)))
    
    ### save output
    saveRDS(out, file=paste0(destDir, "/", varName, "_", user.region.name, "_regions.rds"))
    
}