read_sea_surface_mask <- function() {
    
    #### read in data
    inName <- paste0("input/ecmwf_sst_1979.nc") 
    
    ### open nc file
    nc <- nc_open(inName)
    
    ### get the variables
    lon <- ncvar_get(nc, "longitude")
    lat <- ncvar_get(nc, "latitude")
    
    ### get length
    nlon <- length(lon)
    nlat <- length(lat)
    
    ### read in the 3d file
    tmp_array <- ncvar_get(nc,"sst")
    
    ### create a lonlat file
    lonlat <- as.matrix(expand.grid(lon,lat))
    
    ### use the first date and time to create a storage df
    tmp_slice <- tmp_array[,,1]
    tmp_vec <- as.vector(tmp_slice)
    
    ### convert into DF
    tmpDF <- data.frame(cbind(lonlat,tmp_vec))
    names(tmpDF) <- c("lon","lat","ssf")
    
    tmpDF$ssf <- ifelse(tmpDF$ssf > 0, 1, 0)
    
    tmpDF$lon2 <- ifelse(tmpDF$lon >180, (tmpDF$lon - 360), tmpDF$lon)
    
    ### close the nc
    nc_close(nc)
    
    ### prepare dataset to offset
    out <- tmpDF[,c("lon2", "lat", "ssf")]
    colnames(out) <- c("lon", "lat", "ssf")
    
    out$ssf <- ifelse(is.na(out$ssf), "land", "1")
    out <- out[out$ssf == "land",]
    
    coordinates(out) <- ~lon+lat
    gridded(out) <- T
    r2 <- raster(out)
    
    ### return a df
    return(r2)
}