make_spatial_plots <- function(sourceDir, destDir,
                               user.region.name,
                               date.of.interest,
                               user.lat.max,
                               user.lat.min,
                               user.lon.max,
                               user.lon.min,
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
                                      user.region.name, "_regions.rds"))
    
    drought.intensity <- readRDS(paste0(sourceDir, 
                                      "/drought_intensity_", 
                                      date.of.interest, "_",
                                      drought.duration, "_",
                                      user.region.name, "_regions.rds"))
    
    storm.severity <- readRDS(paste0(sourceDir, 
                                      "/storm_severity_", 
                                      date.of.interest, "_",
                                      storm.duration, "_",
                                     user.region.name, "_regions.rds"))
    
    drought.severity <- readRDS(paste0(sourceDir, 
                                     "/drought_severity_", 
                                     date.of.interest, "_",
                                     drought.duration, "_",
                                     user.region.name, "_regions.rds"))
    
    
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
    latlonDF.sub <- latlonDF[latlonDF$lat<=user.lat.max & latlonDF$lat >= user.lat.min & 
                                 latlonDF$lon <= user.lon.max & latlonDF$lon>=user.lon.min,]
    
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
    
    
    ### read in sea surface mask
    ssf.raster <- read_sea_surface_mask()
    DF1 <- latlonDF.sub[,c("lon", "lat")]
    ssfDF <- cbind(DF1, extract(ssf.raster, DF1, df=T))
    
    ### merge ssf and input DF and then remove sea surface
    drought.severity.long <- merge(drought.severity.long, ssfDF, by=c("lon", "lat"), all=T)
    drought.severity.long <- subset(drought.severity.long, ssf == 1)
    
    storm.severity.long <- merge(storm.severity.long, ssfDF, by=c("lon", "lat"), all=T)
    storm.severity.long <- subset(storm.severity.long, ssf == 1)
    
    drought.intensity.long <- merge(drought.intensity.long, ssfDF, by=c("lon", "lat"), all=T)
    drought.intensity.long <- subset(drought.intensity.long, ssf == 1)
    
    storm.intensity.long <- merge(storm.intensity.long, ssfDF, by=c("lon", "lat"), all=T)
    storm.intensity.long <- subset(storm.intensity.long, ssf == 1)
    
    ### prepare color palette
    n.discrete.colors <- 9
    heat.color <- rev(brewer.pal(n = n.discrete.colors, name = "YlOrRd"))
    rain.color <- rev(brewer.pal(n = n.discrete.colors, name = "Blues"))
    
    ### make categorical bins for the rainfall intensity datasets
    List1 <- convert_continuous_to_discrete_bins(inDF=storm.intensity.long, 
                                                 n.discrete.colors=n.discrete.colors)
    storm.intensity.long <- List1$outDF
    storm.intensity.long.lab <- List1$lab
    n.discrete.colors.storm.intensity <- length(storm.intensity.long.lab)
    rain.color.storm <- rev(brewer.pal(n = n.discrete.colors.storm.intensity, name = "Blues"))
    
    List2 <- convert_continuous_to_discrete_bins(inDF=drought.intensity.long, 
                                                 n.discrete.colors=n.discrete.colors)
    drought.intensity.long <- List2$outDF
    drought.intensity.long.lab <- List2$lab
    n.discrete.colors.drought.intensity <- length(drought.intensity.long.lab)
    rain.color.drought <- rev(brewer.pal(n = n.discrete.colors.drought.intensity, name = "Blues"))
    
    
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
              legend.position="bottom",
              legend.box = 'vertical',
              legend.box.just = 'left')+
        scale_fill_manual(name="value",
                          limits=c("99.9", "99", "95", "90", "80", "70", "60", "50", "40"),
                          values=rain.color,
                          labels=c("99.9", "99", "95", "90", "80", "70", "60", "50", "40"))+
        ggtitle(paste0("Storm ", storm.duration, " severity percentile"))+
        guides(color = guide_legend(nrow=5, byrow = T))
    
    ### plot storm intensity
    p2 <- ggplot(storm.intensity.long, aes(lon, lat)) +
        geom_raster(aes(fill=value_cat))+
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
        ggtitle(paste0("Storm ", storm.duration, " intensity"))+
        scale_fill_manual(name="value",
                          values=rev(rain.color.storm),
                          labels=storm.intensity.long.lab)+
        guides(color = guide_legend(nrow=5, byrow = T))
    
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
              legend.position="bottom",
              legend.box = 'vertical',
              legend.box.just = 'left')+
        scale_fill_manual(name="value",
                          limits=c("0.1", "1", "5", "10", "20", "30", "40", "50", "60"),
                          values=heat.color,
                          labels=c("0.1", "1", "5", "10", "20", "30", "40", "50", "60"))+
        ggtitle(paste0("Antecedent ", drought.duration, " rainfall severity percentile"))+
        guides(color = guide_legend(nrow=5, byrow = T))
    
    
    ### plot drought intensity
    p4 <- ggplot(drought.intensity.long, aes(lon, lat)) +
        geom_raster(aes(fill=value_cat))+
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
        scale_fill_manual(name="value",
                          values=rev(rain.color.drought),
                          labels=drought.intensity.long.lab)+
        ggtitle(paste0("Antecedent ", drought.duration, " rainfall"))+
        guides(color = guide_legend(nrow=5, byrow = T))
    
    
    jpeg(paste0(destDir, "/", user.region.name, "_", date.of.interest,
               "_storm_", storm.duration,
               "_drought_", drought.duration, ".jpg"), units="in", res=150,width = 16, height=16)
    
    
    grid.arrange(p1, p2, p3, p4, nrow = 2)
    
    dev.off()
    
}
