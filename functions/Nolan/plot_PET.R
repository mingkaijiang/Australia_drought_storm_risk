plot_PET <- function(sourceDir, destDir) {
    
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
    
    ### prepare two outDFs
    outDF1 <- c()
    outDF2 <- c()
    
    for (file.id in file.list) {
        
        myData1 <- readRDS(paste0(sourceDir, "/pet_NSW", 
                                  file.list[file.id], "_regions_2d.rds"))
        
        myData2 <- readRDS(paste0(sourceDir, "/rain_NSW", 
                                  file.list[file.id], "_regions_2d.rds"))
        
        ### dimension information
        dim1 <- dim(myData1)[1]
        dim2 <- dim(myData1)[2]
        
        ### two year period
        e <- dim2 - 2
        s <- e - 23
        
        subDF1 <- myData1[,s:e]
        subDF2 <- myData2[,s:e]
        
        sumDF1 <- rowSums(subDF1, na.rm=T, dims=1)
        sumDF2 <- rowSums(subDF2, na.rm=T, dims=1)
        
        tmp1 <- matrix(sumDF1,nrow=100, ncol=160)
        tmp2 <- matrix(sumDF2,nrow=100, ncol=160)
        
        #test <- raster(tmp)
        #plot(test)
        
        ### add group information to split the DF to make it smaller
        latlonDF.sub1 <- latlonDF[latlonDF$lat<=nswDF$lat.start[file.id] & 
                                     latlonDF$lat >= nswDF$lat.end[file.id] & 
                                     latlonDF$lon <= nswDF$lon.end[file.id] & 
                                     latlonDF$lon >= nswDF$lon.start[file.id],]
        
        latlonDF.sub2 <- latlonDF[latlonDF$lat<=nswDF$lat.start[file.id] & 
                                      latlonDF$lat >= nswDF$lat.end[file.id] & 
                                      latlonDF$lon <= nswDF$lon.end[file.id] & 
                                      latlonDF$lon >= nswDF$lon.start[file.id],]
        
        latlonDF.sub1$PET <- as.vector(t(tmp1))
        latlonDF.sub2$Rain <- as.vector(t(tmp2))
        
        outDF1 <- rbind(outDF1, latlonDF.sub1)
        outDF2 <- rbind(outDF2, latlonDF.sub2)
        
    }
    
    outDF1$PET <- outDF1$PET / 2
    outDF2$Rain <- outDF2$Rain / 2
    
    saveRDS(outDF1, paste0(destDir, "/NSW_PET_annual_average_2018_2020.rds"))
    saveRDS(outDF2, paste0(destDir, "/NSW_Rain_annual_average_2018_2020.rds"))
    
    
    
    
    nsw.shp <- readOGR(dsn = file.path("input/NSW_LGA_POLYGON_shp.shp"), stringsAsFactors = F)
    
    e <- extent(nsw.lon.min, nsw.lon.max,
                nsw.lat.min, nsw.lat.max)
    
    crp <- crop(nsw.shp,e)
    
    simp <- gSimplify(crp, 
                      tol = 0.05, 
                      topologyPreserve = TRUE)
    

    ### plot PET
    
    brks <- c(1000, 2000, 3000, 4000, 5000)
    labs <- round(brks, 0)
    
    p1 <- ggplot() +
        geom_tile(outDF1, mapping=aes(lon, lat, fill=PET))+
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
        scale_fill_continuous(name=expression("PET (mm " * yr^-1 * ")"),
                              type="viridis",
                              breaks=brks,
                              label=labs)+
        ggtitle(paste0("Annual mean PET (mm) over 2018-2020"))+
        xlim(nsw.lon.min, nsw.lon.max)+
        ylim(nsw.lat.min, nsw.lat.max)
    
    
    
    brks <- c(0, 250, 500, 750, 1000, 1500, 2000)
    labs <- round(brks, 0)
    
    p2 <- ggplot() +
        geom_tile(outDF2, mapping=aes(lon, lat, fill=Rain))+
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
        scale_fill_continuous(name=expression("Rain (mm " * yr^-1 * ")"),
                              type="viridis",
                              breaks=brks,
                              label=labs)+
        ggtitle(paste0("Annual mean Rainfall (mm) over 2018-2020"))+
        xlim(nsw.lon.min, nsw.lon.max)+
        ylim(nsw.lat.min, nsw.lat.max)
    
    
    ### plot PET
    pdf(paste0(destDir, "/NSW_PET_annual_average_2018_2020.pdf"), width=10, height=8)
    plot(p1)
    dev.off()
    
    pdf(paste0(destDir, "/NSW_Rain_annual_average_2018_2020.pdf"), width=10, height=8)
    plot(p2)
    dev.off()
    
    
}