make_spatial_plots <- function(sourceDir, destDir,
                               inFile,
                               date.of.interest,
                               storm.duration,
                               drought.duration) {
    
    #### Create output folder
    if(!dir.exists(destDir)) {
        dir.create(destDir, showWarnings = FALSE)
    }
    
    #### read in data
    storm.intensity <- readRDS(paste0(sourceDir, 
                                      "/storm_intensity_", 
                                      date.of.interest, "_",
                                      storm.duration, "_",
                                      inFile))
    
    drought.intensity <- readRDS(paste0(sourceDir, 
                                      "/drought_intensity_", 
                                      date.of.interest, "_",
                                      drought.duration, "_",
                                      inFile))
    
    storm.severity <- readRDS(paste0(sourceDir, 
                                      "/storm_severity_", 
                                      date.of.interest, "_",
                                      storm.duration, "_",
                                      inFile))
    
    drought.severity <- readRDS(paste0(sourceDir, 
                                     "/drought_severity_", 
                                     date.of.interest, "_",
                                     drought.duration, "_",
                                     inFile))
    
    
    ### prepare lat and lon real information
    ### grid information
    lat.id <- c(1:691)
    lat.lab <- paste0("lat", lat.id)
    
    lon.id <- c(1:886)
    lon.lab <- paste0("lon", lon.id)
    
    lon <- seq(111.975, 111.975 + (0.05 * 885), by=0.05)
    lat <- seq(-44.525, -44.525 + (0.05 * 690), by=0.05)
    
    ### create lon lat DF for future plotting
    latlonDF <- data.frame(rep(lat.id, each = max(lon.id)),
                           rep(lon.id, max(lat.id)), 
                           rep(lat, each = max(lon.id)),
                           rep(lon, max(lat.id)))
    colnames(latlonDF) <- c("latID", "lonID", "lat", "lon")
    
    ### add group information to split the DF to make it smaller
    latlonDF.sub <- latlonDF[latlonDF$lat<=-28 & latlonDF$lat >= -36 & latlonDF$lon <= 155 & latlonDF$lon>=150,]
    
    ### get subset lat information
    lat.list.sub <- unique(latlonDF.sub$latID)
    lat.length <- length(lat.list.sub)
    lat.id <- 1:lat.length
    lat <- unique(latlonDF$lat)
    latlonDF.sub$latID <- latlonDF.sub$latID - (min(latlonDF.sub$latID) - 1)
    
    lon.list.sub <- unique(latlonDF.sub$lonID)
    lon.length <- length(lon.list.sub)
    lon.id <- 1:lon.length
    lon <- unique(latlonDF$lon)
    latlonDF.sub$lonID <- latlonDF.sub$lonID - (min(latlonDF.sub$lonID) - 1)
    
    ### Make spatial plot DFs
    drought.severity.long <- melt(drought.severity)
    colnames(drought.severity.long) <- c("latID", "lonID", "value")
    drought.severity.long <- merge(drought.severity.long, latlonDF.sub,
                                   by=c("latID", "lonID"))
    
    storm.severity.long <- melt(storm.severity)
    colnames(storm.severity.long) <- c("latID", "lonID", "value")
    storm.severity.long <- merge(storm.severity.long, latlonDF.sub,
                                   by=c("latID", "lonID"))
    
    drought.intensity.long <- melt(drought.intensity)
    colnames(drought.intensity.long) <- c("latID", "lonID", "value")
    drought.intensity.long <- merge(drought.intensity.long, latlonDF.sub,
                                   by=c("latID", "lonID"))
    
    storm.intensity.long <- melt(storm.intensity)
    colnames(storm.intensity.long) <- c("latID", "lonID", "value")
    storm.intensity.long <- merge(storm.intensity.long, latlonDF.sub,
                                    by=c("latID", "lonID"))
    
    
    ### prepare color palette
    heat.color <- rev(brewer.pal(n = 9, name = "YlOrRd"))
    rain.color <- rev(brewer.pal(n = 9, name = "Blues"))
    
    
    #### ploting storm severity
    p1 <- ggplot(storm.severity.long, aes(lon, lat)) +
        geom_raster(aes(fill=as.character(value)))+
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
              legend.position="right",
              legend.box = 'vertical',
              legend.box.just = 'left')+
        scale_fill_manual(name="value",
                          limits=c("99.9", "99", "95", "90", "80", "70", "60", "50", "40"),
                          values=rain.color,
                          labels=c("99.9", "99", "95", "90", "80", "70", "60", "50", "40"))+
        ggtitle("Storm severity percentile")
    
    ### plot storm intensity
    p2 <- ggplot(storm.intensity.long, aes(lon, lat)) +
        geom_raster(aes(fill=value))+
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
              legend.position="right",
              legend.box = 'vertical',
              legend.box.just = 'left')+
        ggtitle(paste0("Storm ", storm.duration, " intensity"))
    
    #### ploting drought severity
    p3 <- ggplot(drought.severity.long, aes(lon, lat)) +
        geom_raster(aes(fill=as.character(value)))+
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
              legend.position="right",
              legend.box = 'vertical',
              legend.box.just = 'left')+
        scale_fill_manual(name="value",
                          limits=c("0.1", "1", "5", "10", "20", "30", "40", "50", "60"),
                          values=heat.color,
                          labels=c("0.1", "1", "5", "10", "20", "30", "40", "50", "60"))+
        ggtitle("Drought severity percentile")
    
    
    ### plot drought intensity
    p4 <- ggplot(drought.intensity.long, aes(lon, lat)) +
        geom_raster(aes(fill=value))+
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
              legend.position="right",
              legend.box = 'vertical',
              legend.box.just = 'left')+
        ggtitle(paste0("Antecedent ", drought.duration, " rainfall"))
    
    
    pdf(paste0(destDir, "/", inFile, ".pdf"), width = 8, height=8)
    plot(p1)
    plot(p2)
    plot(p3)
    plot(p4)
    
    dev.off()
    
}