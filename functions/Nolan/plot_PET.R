plot_PET <- function(sourceDir, destDir) {
    
    
    sourceDir <- "input"
    destDir <- "output/Nolan"
    
    
    nsw.lon.max = 156
    nsw.lon.min = 140
    nsw.lat.max = -28
    nsw.lat.min = -38
    
    nsw.lat.list.s <- seq(nsw.lat.max, (nsw.lat.min+5), by=-5)
    nsw.lon.list.s <- seq(nsw.lon.min, (nsw.lon.max-8), by=8)
    nsw.lat.list.e <- seq((nsw.lat.max-5), nsw.lat.min, by=-5)
    nsw.lon.list.e <- seq((nsw.lon.min+8), nsw.lon.max, by=8)
    
    nswDF <- data.frame("lon.start" = rep(nsw.lon.list.s, each=length(nsw.lat.list.s)),
                        "lon.end" = rep(nsw.lon.list.e, each=length(nsw.lat.list.e)),
                        "lat.start" = rep(nsw.lat.list.s, length(nsw.lon.list.s)),
                        "lat.end" = rep(nsw.lat.list.e, length(nsw.lon.list.e)))
    
    
    #### Create output folder
    if(!dir.exists(destDir)) {
        dir.create(destDir, showWarnings = FALSE)
    }
    
    ### get number of files
    n <- nrow(nswDF)
    
    ### there are n DFs available
    file.list <- c(1:n)
    
    ### prepare two outDFs
    outDF <- c()
    
    #nswDF$lon.end[c(7:9)] <- 156.225
    
    
    ### expand the array to add lat and lon information
    ### grid information
    lat.id <- c(1:691)
    lat.lab <- paste0("lat", lat.id)
    
    lon.id <- c(1:886)
    lon.lab <- paste0("lon", lon.id)
    
    lon <- seq(111.975, 111.975 + (0.05 * 885), by=0.05)
    lat <- seq(-10.025, -10.025 + (-0.05 * 690), by=-0.05)
    
    ### create lon lat DF for future plotting
    latlonDF <- data.frame(rep(lat.id, each = max(lon.id)),
                           rep(lon.id, max(lat.id)), 
                           rep(lat, each = max(lon.id)),
                           rep(lon, max(lat.id)))
    colnames(latlonDF) <- c("latID", "lonID", "lat", "lon")
    
    for (file.id in file.list[c(1:2)]) {
        
        #file.id = 6
        
        myData <- readRDS(paste0(sourceDir, "/pet_NSW", 
                                 file.list[file.id], "_regions.rds"))
        
        ### dimension information
        dim1 <- dim(myData)[1]
        dim2 <- dim(myData)[2]
        
        subDF <- myData[,1]
        
        tmp <- matrix(subDF,nrow=100, ncol=160)
        
        #test <- raster(tmp)
        #plot(test)
        
        ### add group information to split the DF to make it smaller
        latlonDF.sub <- latlonDF[latlonDF$lat<=nswDF$lat.start[file.id] & 
                                     latlonDF$lat >= nswDF$lat.end[file.id] & 
                                     latlonDF$lon <= nswDF$lon.end[file.id] & 
                                     latlonDF$lon >= nswDF$lon.start[file.id],]
        
        latlonDF.sub$PD <- as.vector(t(tmp))

        outDF <- rbind(outDF, latlonDF.sub)
        
    }
    
    
    nsw.shp <- readOGR(dsn = file.path("input/NSW_LGA_POLYGON_shp.shp"), stringsAsFactors = F)
    
    e <- extent(nsw.lon.min, nsw.lon.max,
                nsw.lat.min, nsw.lat.max)
    
    crp <- crop(nsw.shp,e)
    
    simp <- gSimplify(crp, 
                      tol = 0.05, 
                      topologyPreserve = TRUE)
    
    #d1 <- d2 <- sqrt(dim1)
    #test <- array(unlist(myData), dim = c(d1,d2,dim2))
    #test1 <- test[,,1]
    #r1 <- raster(test1)
    #plot(r1)
    
    
    #plotDF <- outDF[1:16000,]
    p7 <- ggplot() +
        geom_tile(outDF, mapping=aes(lon, lat, fill=PD))+
        geom_polygon(data = simp, 
                     aes(x = long, y = lat, group = group), 
                     colour = "black", fill = NA)+
        geom_sf(fill=NA) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              legend.box = 'vertical',
              legend.box.just = 'left'); p7
    
}