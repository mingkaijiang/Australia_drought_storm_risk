process_GSOD_station_data <- function(sourceDir, destDir,
                                      user.region.name,
                                      user.lat.max,
                                      user.lat.min,
                                      user.lon.max,
                                      user.lon.min,
                                      plot.option) {
    
    
    ### select station list based on user defined region
    gsodDF <- select_station_from_GSOD_global_dataset(destDir=destDir,
                                                      user.region.name=user.region.name,
                                                      user.lat.max=user.lat.max,
                                                      user.lat.min=user.lat.min,
                                                      user.lon.max=user.lon.max,
                                                      user.lon.min=user.lon.min,
                                                      plot.option=plot.option)
    

    ### loop through the folders to find stations based on the user defined station list
    
    
    ### create data path
    gsodDF.path <- create_data_path_for_GSOD_dataset(inDF=gsodDF)
    
}