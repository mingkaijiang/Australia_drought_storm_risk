read_Australia_polygon <- function() {

    ### read country Australia
    aus <- ne_countries(scale = "medium", country = "Australia", returnclass = "sf")
    
    gridsize <- 0.05
    r <- raster(extent(aus), res=gridsize)
    rr <- rasterize(aus, r)
    
    
    return(rr)
}