merge_Australia_storm_extreme_DF <- function(sourceDir, destDir, 
                                             duration) {
    
    
    ### prepare storage DF
    stDF <- array(NA, c(691, 886, 5))
    
    ### allocate splitted data to the whole dataset
    for (i in 1:23) {
        
        ### read in the data
        myData <- readRDS(paste0(sourceDir, "/Group_", i, 
                                 "_Storm_extreme_percentile_", duration,
                                 "_Australia.rds"))
        
        dim <- dim(myData)[1]
        
        ### location index
        loc1 <- (i-1)*30 + 1
        loc2 <- loc1 + dim - 1
        
        ### assign values
        stDF[loc1:loc2,,] <- myData
    }
    
    ### save
    saveRDS(stDF, file=paste0(destDir, "/Storm_extreme_percentile_", duration,
                                        "_Australia.rds"))
    
    ########################### prepare grid information DF ############################
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
    
    ### add group information to split the DF to make it smaller
    latlonDF$Group <- c(rep(c(1:23), each = 886 * 30), 
                        rep(23, each=886 * 1))
    
    ########################### end grid information DF ############################
    
    
    ### prepare dataframe to be plotted
    ## P999
    subDF1 <- stDF[,,1] 
    plotDF1 <- melt(subDF1)
    colnames(plotDF1) <- c("latID", "lonID", "value")
    plotDF1 <- merge(plotDF1, latlonDF, by=c("latID", "lonID"))
    
    ## P99
    subDF2 <- stDF[,,12] 
    plotDF2 <- melt(subDF2)
    colnames(plotDF2) <- c("latID", "lonID", "value")
    plotDF2 <- merge(plotDF2, latlonDF, by=c("latID", "lonID"))
    
    ## P95
    subDF3 <- stDF[,,3] 
    plotDF3 <- melt(subDF3)
    colnames(plotDF3) <- c("latID", "lonID", "value")
    plotDF3 <- merge(plotDF3, latlonDF, by=c("latID", "lonID"))
    
    ## P90
    subDF4 <- stDF[,,4] 
    plotDF4 <- melt(subDF4)
    colnames(plotDF4) <- c("latID", "lonID", "value")
    plotDF4 <- merge(plotDF4, latlonDF, by=c("latID", "lonID"))
    
    
    ### read in Australia
    aus <- read_Australia_polygon()
    DF1 <- latlonDF[,c("lon", "lat")]
    ausDF <- cbind(DF1, extract(aus, DF1, df=T))
    
    ### merge australia raster and input DF and then remove sea surface
    plotDF1 <- merge(plotDF1, ausDF, by=c("lon", "lat"), all=T)
    plotDF1 <- subset(plotDF1, layer == 1)
    plotDF1 <- subset(plotDF1, value != "NA")
    
    plotDF2 <- merge(plotDF2, ausDF, by=c("lon", "lat"), all=T)
    plotDF2 <- subset(plotDF2, layer == 1)
    plotDF2 <- subset(plotDF2, value != "NA")
    
    plotDF3 <- merge(plotDF3, ausDF, by=c("lon", "lat"), all=T)
    plotDF3 <- subset(plotDF3, layer == 1)
    plotDF3 <- subset(plotDF3, value != "NA")
    
    plotDF4 <- merge(plotDF4, ausDF, by=c("lon", "lat"), all=T)
    plotDF4 <- subset(plotDF4, layer == 1)
    plotDF4 <- subset(plotDF4, value != "NA")
    
    ### prepare color palette
    n.discrete.colors <- 9
    heat.color <- rev(brewer.pal(n = n.discrete.colors, name = "YlOrRd"))
    rain.color <- rev(brewer.pal(n = n.discrete.colors, name = "Blues"))
    
    ### australia polygon
    aus.poly <- ne_countries(scale = "medium", country = "Australia", returnclass = "sf")
    
    
    #### ploting storm severity
    p1 <- ggplot(aus.poly) +
        geom_tile(plotDF1, mapping=aes(lon, lat, fill=value))+
        geom_sf(fill=NA) +
        geom_point(aes(x=151.2093, y=-33.8688), col="red")+  # sydney
        annotate("text", x=151.2093, y=-34.2, label = "Sydney")+
        geom_point(aes(x=149.13, y=-35.2809), col="red")+    # canberra
        annotate("text", x=149.13, y=-35.5, label = "Canberra")+
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
              legend.box.just = 'left')+
        scale_fill_viridis_b(name="value")+
        ggtitle(paste0("Storm ", storm.duration, " severity P99.9"))+
        guides(color = guide_legend(nrow=5, byrow = T))
    
    p2 <- ggplot(aus.poly) +
        geom_tile(plotDF2, mapping=aes(lon, lat, fill=value))+
        geom_sf(fill=NA) +
        geom_point(aes(x=151.2093, y=-33.8688), col="red")+  # sydney
        annotate("text", x=151.2093, y=-34.2, label = "Sydney")+
        geom_point(aes(x=149.13, y=-35.2809), col="red")+    # canberra
        annotate("text", x=149.13, y=-35.5, label = "Canberra")+
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
              legend.box.just = 'left')+
        scale_fill_viridis_b(name="value")+
        ggtitle(paste0("Storm ", storm.duration, " severity P99"))+
        guides(color = guide_legend(nrow=5, byrow = T))
    
    p3 <- ggplot(aus.poly) +
        geom_tile(plotDF3, mapping=aes(lon, lat, fill=value))+
        geom_sf(fill=NA) +
        geom_point(aes(x=151.2093, y=-33.8688), col="red")+  # sydney
        annotate("text", x=151.2093, y=-34.2, label = "Sydney")+
        geom_point(aes(x=149.13, y=-35.2809), col="red")+    # canberra
        annotate("text", x=149.13, y=-35.5, label = "Canberra")+
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
              legend.box.just = 'left')+
        scale_fill_viridis_b(name="value")+
        ggtitle(paste0("Storm ", storm.duration, " severity P95"))+
        guides(color = guide_legend(nrow=5, byrow = T))
    
    p4 <- ggplot(aus.poly) +
        geom_tile(plotDF4, mapping=aes(lon, lat, fill=value))+
        geom_sf(fill=NA) +
        geom_point(aes(x=151.2093, y=-33.8688), col="red")+  # sydney
        annotate("text", x=151.2093, y=-34.2, label = "Sydney")+
        geom_point(aes(x=149.13, y=-35.2809), col="red")+    # canberra
        annotate("text", x=149.13, y=-35.5, label = "Canberra")+
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
              legend.box.just = 'left')+
        scale_fill_viridis_b(name="value")+
        ggtitle(paste0("Storm ", storm.duration, " severity P90"))+
        guides(color = guide_legend(nrow=5, byrow = T))
    
    
    ### save plot
    jpeg(paste0(destDir, "/Australia_", date.of.interest,
                "_storm_", storm.duration,
                "_percentile.jpg"), units="in", res=150,width = 16, height=16)
    
    grid.arrange(p1, p2, p3, p4, nrow = 2)
    
    dev.off()
    
    
}