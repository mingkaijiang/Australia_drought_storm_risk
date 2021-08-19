merge_water_deficit_dataframes_and_plot_Nolan <- function(sourceDir, 
                                                          destDir,
                                                          nswDF) {
    
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
    
    nswDF$lon.end[c(7:9)] <- 156.225
    
    
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
    
    
    ### read in the R database
    for (file.id in file.list) {
        myData <- readRDS(paste0(sourceDir, "/water_deficit_percentile_period_of_interest_NSW", 
                                  file.list[file.id], "_regions.rds"))

        ### dimension information
        dim1 <- dim(myData)[1]
        dim2 <- dim(myData)[2]
        
        subDF1 <- myData[,1]
        subDF2 <- myData[,2]
        
        lon1 <- length(seq(nswDF$lon.start[file.id]+0.025, 
                           nswDF$lon.end[file.id], by=0.05))
        lat1 <- length(seq(nswDF$lat.start[file.id]-0.025, 
                           nswDF$lat.end[file.id], by=-0.05))
        
        
        tmp1 <- matrix(subDF1,nrow=lon1, ncol=lat1)
        tmp2 <- matrix(subDF2,nrow=lon1, ncol=lat1)
        
        ### lon lat information
        #lon.min <- min(nswDF$lon.start[file.id]) + 0.025
        #lon.max <- max(nswDF$lon.end[file.id])
        #lat.min <- min(nswDF$lat.end[file.id]) + 0.025
        #lat.max <- max(nswDF$lat.start[file.id])
        #
        #lon <- seq(lon.min, lon.max, by=0.05)
        #lat <- rev(seq(lat.min, lat.max, by=0.05))
        #
        #tmpDF <- data.frame("Lon" = rep(lon, length(lat)),
        #                    "Lat" = rep(lat, each=length(lon)),
        #                    "PD" = subDF1,
        #                    "PD_percent" = subDF2)
        
        #outDF <- rbind(outDF, tmpDF)
        
        ### add group information to split the DF to make it smaller
        latlonDF.sub <- latlonDF[latlonDF$lat<=nswDF$lat.start[file.id] & 
                                     latlonDF$lat >= nswDF$lat.end[file.id] & 
                                     latlonDF$lon <= nswDF$lon.end[file.id] & 
                                     latlonDF$lon >= nswDF$lon.start[file.id],]
        
        latlonDF.sub$PD <- as.vector(t(tmp1))
        latlonDF.sub$PD_percent <- as.vector(t(tmp2))
        
        outDF <- rbind(outDF, latlonDF.sub)

    } # file.id


    
    
    
    
    
    
        
    ### to do list: check lon list order issue
    ###             assign values to the lon lat list
    ###             merge all
    ###             plot two maps: real magnitude, percentile
    ###             check spatial pattern, and value magnitude to see if they make sense.
    
    
    
    ### convert into 3d matrix
    #d1 <- d2 <- sqrt(dim1)
    #test <- array(unlist(myData), dim = c(d1,d2,dim2))
    #test1 <- test[,,1]
    #r1 <- raster(test1)
    #plot(r1)

    nsw.shp <- readOGR(dsn = file.path("input/NSW_LGA_POLYGON_shp.shp"), stringsAsFactors = F)
    
    nsw.lon.max = 162
    nsw.lon.min = 138
    nsw.lat.max = -14
    nsw.lat.min = -38
    
    e <- extent(nsw.lon.min, nsw.lon.max,
                nsw.lat.min, nsw.lat.max)
    
    crp <- crop(nsw.shp,e)
    
    simp <- gSimplify(crp, 
                      tol = 0.05, 
                      topologyPreserve = TRUE)
    
    
    p6 <- ggplot() +
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
              legend.box.just = 'left'); p6#+
    #scale_fill_manual(name="value",
    #                  values=rev(rain.color.drought.1yr),
    #                  labels=drought.intensity.1yr.long.lab)+
    #ggtitle(paste0("Antecedent 1-year water availability (mm)"))+
    #guides(fill = guide_legend(nrow=2, byrow = T))+
    #xlim(user.lon.min, user.lon.max)+
    #ylim(user.lat.min, user.lat.max)

    
    p7 <- ggplot() +
        #geom_tile(latlonDF.sub, mapping=aes(lon, lat, fill=PD))+
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
    
    plot(p7)
    
}  