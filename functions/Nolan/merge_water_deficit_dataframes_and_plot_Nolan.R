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
    
    
    ### read in the R database
    for (file.id in file.list) {
        
        ### water deficit DF
        myData <- readRDS(paste0(sourceDir, "/water_deficit_percentile_period_of_interest_NSW", 
                                  file.list[file.id], "_regions.rds"))
        
        ### dimension information
        dim1 <- dim(myData)[1]
        dim2 <- dim(myData)[2]
        
        subDF1 <- myData[,1]
        subDF2 <- myData[,2]
        
        #tmp <- matrix(subDF,nrow=100, ncol=160)
        #test <- raster(tmp)
        #plot(test)
        
        lon1 <- length(seq(nswDF$lon.start[file.id]+0.025, 
                           nswDF$lon.end[file.id], by=0.05))
        lat1 <- length(seq(nswDF$lat.start[file.id]-0.025, 
                           nswDF$lat.end[file.id], by=-0.05))
        
        
        tmp1 <- matrix(subDF1,nrow=lat1, ncol=lon1)
        tmp2 <- matrix(subDF2,nrow=lat1, ncol=lon1)
        
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


    ### convert into 3d matrix
    #d1 <- d2 <- sqrt(dim1)
    #test <- array(unlist(myData), dim = c(d1,d2,dim2))
    #test1 <- test[,,1]
    #r1 <- raster(test1)
    #plot(r1)

    
    ### calculate one-yr average
    outDF$PD <- outDF$PD / 2
    
    
    ### save
    saveRDS(outDF, paste0(destDir, "/NSW_P-PET_annual_average_2018_2020.rds"))
    
    
    ### prepare NSW polygon
    nsw.shp <- readOGR(dsn = file.path("input/NSW_LGA_POLYGON_shp.shp"), stringsAsFactors = F)
    
    nsw.lon.max = 156
    nsw.lon.min = 140
    nsw.lat.max = -28
    nsw.lat.min = -38
    
    e <- extent(nsw.lon.min, nsw.lon.max,
                nsw.lat.min, nsw.lat.max)
    
    crp <- crop(nsw.shp,e)
    
    simp <- gSimplify(crp, 
                      tol = 0.05, 
                      topologyPreserve = TRUE)
    
    
    ### prepare plot
    brks <- c(-5000, -2500, 0, 1000)
    labs <- round(brks, 0)
    
    
    p1 <- ggplot() +
        geom_tile(outDF, mapping=aes(lon, lat, fill=PD))+
        geom_polygon(data = simp, 
                     aes(x = long, y = lat, group = group), 
                     colour = "black", fill = NA)+
        #geom_sf(fill=NA) +
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
              legend.box.just = 'left',
              legend.key.width=unit(2,"cm"))+
        scale_fill_continuous(name=expression("P-PET (mm " * yr^-1 * ")"),
                              type="viridis",
                              breaks=brks,
                              label=labs)+
        ggtitle(paste0("annual mean P-PET (mm) over 2018-2020"))+
    #guides(fill = guide_legend(nrow=2, byrow = T))+
    xlim(nsw.lon.min, nsw.lon.max)+
    ylim(nsw.lat.min, nsw.lat.max)
    
    
    pdf(paste0(destDir, "/NSW_P_PET_annual_average_2018_2020.pdf"), width=10, height=8)
    plot(p1)
    dev.off()
    
    
    log_brks <- quantile(log(outDF$PD_percent), c(0.2, 0.4, 0.6, 0.8, 0.9, 1.0))
    log_labs <- round(exp(log_brks), 3)
    
    p2 <- ggplot() +
        geom_tile(outDF, mapping=aes(lon, lat, fill=log(PD_percent)))+
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
              legend.box.just = 'left',
              legend.key.width=unit(2,"cm"))+
        scale_fill_continuous(name="P-PET (percentile)",
                              type="viridis",
                              breaks=log_brks,
                              label=log_labs)+
        ggtitle(paste0("2-year P-PET percentile over 2018-2020 relative to 1950-2020"))+
        #guides(fill = guide_legend(nrow=2, byrow = T))+
        xlim(nsw.lon.min, nsw.lon.max)+
        ylim(nsw.lat.min, nsw.lat.max)
    
    pdf(paste0(destDir, "/NSW_P_PET_index_2yr_percentile.pdf"), width=10, height=8)
    plot(p2)
    dev.off()
    

}  