make_spatial_plots_for_Sydney_Hunter_valley_regions <- function(sourceDir, destDir,
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
    
    wind.severity <- read.csv(paste0(sourceDir, "/GSOD_Wind_Extreme_Severity_", 
                                     date.of.interest, "_", user.region.name,
                                     "_regions.csv"))
    
    
    ### prepare lat and lon real information
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
    
    ### read in Australia
    aus <- read_Australia_polygon()
    DF1 <- latlonDF[,c("lon", "lat")]
    ausDF <- cbind(DF1, extract(aus, DF1, df=T))
    
    ### merge australia raster and input DF and then remove sea surface
    drought.severity.long <- merge(drought.severity.long, ausDF, by=c("lon", "lat"), all=T)
    drought.severity.long <- subset(drought.severity.long, layer == 1)
    drought.severity.long <- subset(drought.severity.long, value != "NA")
    
    storm.severity.long <- merge(storm.severity.long, ausDF, by=c("lon", "lat"), all=T)
    storm.severity.long <- subset(storm.severity.long, layer == 1)
    storm.severity.long <- subset(storm.severity.long, value != "NA")
    
    drought.intensity.long <- merge(drought.intensity.long, ausDF, by=c("lon", "lat"), all=T)
    drought.intensity.long <- subset(drought.intensity.long, layer == 1)
    drought.intensity.long <- subset(drought.intensity.long, value != "NA")
    
    storm.intensity.long <- merge(storm.intensity.long, ausDF, by=c("lon", "lat"), all=T)
    storm.intensity.long <- subset(storm.intensity.long, layer == 1)
    storm.intensity.long <- subset(storm.intensity.long, value != "NA")
    
    ### lat and lon range for the Sydney and Hunter Valley range
    lat.max.syd <- -32
    lat.min.syd <- -34
    lon.max.syd <- 152
    lon.min.syd <- 150
    
    ### subset Sydney and Hunter Valley region
    #drought.severity.long <- subset(drought.severity.long, lat <= lat.max.syd & lat >= lat.min.syd &
    #                                    lon <= lon.max.syd & lon >= lon.min.syd)
    #
    #storm.severity.long <- subset(storm.severity.long, lat <= lat.max.syd & lat >= lat.min.syd &
    #                                    lon <= lon.max.syd & lon >= lon.min.syd)
    #
    #drought.intensity.long <- subset(drought.intensity.long, lat <= lat.max.syd & lat >= lat.min.syd &
    #                                    lon <= lon.max.syd & lon >= lon.min.syd)
    #
    #storm.intensity.long <- subset(storm.intensity.long, lat <= lat.max.syd & lat >= lat.min.syd &
    #                                    lon <= lon.max.syd & lon >= lon.min.syd)
    
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
    
    ### australia polygon
    aus.poly <- ne_countries(scale = "medium", country = "Australia", returnclass = "sf")
    
    
    #### ploting storm severity
    p1 <- ggplot(aus.poly) +
        geom_raster(storm.severity.long, mapping=aes(lon, lat, fill=as.character(value)))+
        geom_sf(fill=NA) +
        geom_point(aes(x=151.2093, y=-33.8688), col="red")+  # sydney
        annotate("text", x=151.2093, y=-33.9688, label = "Sydney", col="brown")+
        geom_point(aes(x=150.8595, y=-32.0546), col="red")+    # canberra
        annotate("text", x=150.8595, y=-32.1546, label = "Scone", col="brown")+
        geom_point(aes(x=151.7817, y=-32.9283), col="red")+    # new castle
        annotate("text", x=151.7817, y=-33.0, label = "Newcastle", col="brown")+
        geom_point(aes(x=150.3557, y=-32.1393), col="red")+    # Merriwa
        annotate("text", x=150.3557, y=-32.2393, label = "Merriwa", col="brown")+
        geom_point(aes(x=151.3624, y=-32.8345), col="red")+    # Cessnock
        annotate("text", x=151.3624, y=-32.9345, label = "Cessnock", col="brown")+
        geom_point(aes(x=150.75, y=-33.6), col="red")+    # Richmond
        annotate("text", x=150.75, y=-33.7, label = "Richmond", col="brown")+
        geom_point(aes(x=151.1788, y=-32.5695), col="red")+    # Singleton
        annotate("text", x=151.1788, y=-32.6695, label = "Singleton", col="brown")+
        geom_point(aes(x=151.3417, y=-33.4267), col="red")+    # Gosford
        annotate("text", x=151.3417, y=-33.5267, label = "Gosford", col="brown")+
        annotate("text", x=150.9190, y=-33.0482, label = "Yengo NF")+
        annotate("text", x=150.3765, y=-33.0174, label = "Wollemi NF")+
        annotate("text", x=151.6632, y=-32.0671, label = "Barrington Tops NF")+
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
        scale_fill_manual(name="percentile",
                          limits=c("99.9", "99", "95", "90", "80", "70", "60", "50", "40"),
                          values=rain.color,
                          labels=c("99.9", "99", "95", "90", "80", "70", "60", "50", "40"))+
        ggtitle(paste0("Storm ", storm.duration, " severity percentile"))+
        guides(color = guide_legend(nrow=5, byrow = T))+
        xlim(lon.min.syd, lon.max.syd)+
        ylim(lat.min.syd, lat.max.syd)
    
    ### plot storm intensity (mm/d)
    p2 <- ggplot(aus.poly) +
        geom_tile(storm.intensity.long, mapping=aes(lon, lat, fill=value_cat))+
        geom_sf(fill=NA) +
        geom_point(aes(x=151.2093, y=-33.8688), col="red")+  # sydney
        annotate("text", x=151.2093, y=-33.9688, label = "Sydney", col="brown")+
        geom_point(aes(x=150.8595, y=-32.0546), col="red")+    # canberra
        annotate("text", x=150.8595, y=-32.1546, label = "Scone", col="brown")+
        geom_point(aes(x=151.7817, y=-32.9283), col="red")+    # new castle
        annotate("text", x=151.7817, y=-33.0, label = "Newcastle", col="brown")+
        geom_point(aes(x=150.3557, y=-32.1393), col="red")+    # Merriwa
        annotate("text", x=150.3557, y=-32.2393, label = "Merriwa", col="brown")+
        geom_point(aes(x=151.3624, y=-32.8345), col="red")+    # Cessnock
        annotate("text", x=151.3624, y=-32.9345, label = "Cessnock", col="brown")+
        geom_point(aes(x=150.75, y=-33.6), col="red")+    # Richmond
        annotate("text", x=150.75, y=-33.7, label = "Richmond", col="brown")+
        geom_point(aes(x=151.1788, y=-32.5695), col="red")+    # Singleton
        annotate("text", x=151.1788, y=-32.6695, label = "Singleton", col="brown")+
        geom_point(aes(x=151.3417, y=-33.4267), col="red")+    # Gosford
        annotate("text", x=151.3417, y=-33.5267, label = "Gosford", col="brown")+
        annotate("text", x=150.9190, y=-33.0482, label = "Yengo NF")+
        annotate("text", x=150.3765, y=-33.0174, label = "Wollemi NF")+
        annotate("text", x=151.6632, y=-32.0671, label = "Barrington Tops NF")+
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
        ggtitle(paste0("Storm ", storm.duration, " intensity (mm/d)"))+
        scale_fill_manual(name="value",
                          values=rev(rain.color.storm),
                          labels=storm.intensity.long.lab)+
        guides(color = guide_legend(nrow=5, byrow = T))+
        xlim(lon.min.syd, lon.max.syd)+
        ylim(lat.min.syd, lat.max.syd)
    
    #### ploting drought severity
    p3 <- ggplot(aus.poly) +
        geom_tile(drought.severity.long, mapping=aes(lon, lat, fill=as.character(value)))+
        geom_sf(fill=NA) +
        geom_point(aes(x=151.2093, y=-33.8688), col="red")+  # sydney
        annotate("text", x=151.2093, y=-33.9688, label = "Sydney", col="brown")+
        geom_point(aes(x=150.8595, y=-32.0546), col="red")+    # canberra
        annotate("text", x=150.8595, y=-32.1546, label = "Scone", col="brown")+
        geom_point(aes(x=151.7817, y=-32.9283), col="red")+    # new castle
        annotate("text", x=151.7817, y=-33.0, label = "Newcastle", col="brown")+
        geom_point(aes(x=150.3557, y=-32.1393), col="red")+    # Merriwa
        annotate("text", x=150.3557, y=-32.2393, label = "Merriwa", col="brown")+
        geom_point(aes(x=151.3624, y=-32.8345), col="red")+    # Cessnock
        annotate("text", x=151.3624, y=-32.9345, label = "Cessnock", col="brown")+
        geom_point(aes(x=150.75, y=-33.6), col="red")+    # Richmond
        annotate("text", x=150.75, y=-33.7, label = "Richmond", col="brown")+
        geom_point(aes(x=151.1788, y=-32.5695), col="red")+    # Singleton
        annotate("text", x=151.1788, y=-32.6695, label = "Singleton", col="brown")+
        geom_point(aes(x=151.3417, y=-33.4267), col="red")+    # Gosford
        annotate("text", x=151.3417, y=-33.5267, label = "Gosford", col="brown")+
        annotate("text", x=150.9190, y=-33.0482, label = "Yengo NF")+
        annotate("text", x=150.3765, y=-33.0174, label = "Wollemi NF")+
        annotate("text", x=151.6632, y=-32.0671, label = "Barrington Tops NF")+
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
        scale_fill_manual(name="percentile",
                          limits=c("0.1", "1", "5", "10", "20", "30", "40", "50", "60"),
                          values=heat.color,
                          labels=c("0.1", "1", "5", "10", "20", "30", "40", "50", "60"))+
        ggtitle(paste0("Antecedent ", drought.duration, " rainfall severity percentile"))+
        guides(color = guide_legend(nrow=5, byrow = T))+
        xlim(lon.min.syd, lon.max.syd)+
        ylim(lat.min.syd, lat.max.syd)
    
    
    ### plot drought intensity (mm/yr)
    p4 <- ggplot(aus.poly) +
        geom_tile(drought.intensity.long, mapping=aes(lon, lat, fill=value_cat))+
        geom_sf(fill=NA) +
        geom_point(aes(x=151.2093, y=-33.8688), col="red")+  # sydney
        annotate("text", x=151.2093, y=-33.9688, label = "Sydney", col="brown")+
        geom_point(aes(x=150.8595, y=-32.0546), col="red")+    # canberra
        annotate("text", x=150.8595, y=-32.1546, label = "Scone", col="brown")+
        geom_point(aes(x=151.7817, y=-32.9283), col="red")+    # new castle
        annotate("text", x=151.7817, y=-33.0, label = "Newcastle", col="brown")+
        geom_point(aes(x=150.3557, y=-32.1393), col="red")+    # Merriwa
        annotate("text", x=150.3557, y=-32.2393, label = "Merriwa", col="brown")+
        geom_point(aes(x=151.3624, y=-32.8345), col="red")+    # Cessnock
        annotate("text", x=151.3624, y=-32.9345, label = "Cessnock", col="brown")+
        geom_point(aes(x=150.75, y=-33.6), col="red")+    # Richmond
        annotate("text", x=150.75, y=-33.7, label = "Richmond", col="brown")+
        geom_point(aes(x=151.1788, y=-32.5695), col="red")+    # Singleton
        annotate("text", x=151.1788, y=-32.6695, label = "Singleton", col="brown")+
        geom_point(aes(x=151.3417, y=-33.4267), col="red")+    # Gosford
        annotate("text", x=151.3417, y=-33.5267, label = "Gosford", col="brown")+
        annotate("text", x=150.9190, y=-33.0482, label = "Yengo NF")+
        annotate("text", x=150.3765, y=-33.0174, label = "Wollemi NF")+
        annotate("text", x=151.6632, y=-32.0671, label = "Barrington Tops NF")+
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
        ggtitle(paste0("Antecedent ", drought.duration, " rainfall (mm/yr)"))+
        guides(color = guide_legend(nrow=5, byrow = T))+
        xlim(lon.min.syd, lon.max.syd)+
        ylim(lat.min.syd, lat.max.syd)
    
    
    ### wind severity percentile
    p5 <- ggplot(aus.poly) +
        geom_point(wind.severity, mapping=aes(lon, lat, 
                                              fill=as.character(wind.severity.percentile)), 
                   pch = 21, size = 5)+
        geom_sf(fill=NA) +
        geom_point(aes(x=151.2093, y=-33.8688), col="red")+  # sydney
        annotate("text", x=151.2093, y=-33.9688, label = "Sydney", col="brown")+
        geom_point(aes(x=150.8595, y=-32.0546), col="red")+    # canberra
        annotate("text", x=150.8595, y=-32.1546, label = "Scone", col="brown")+
        geom_point(aes(x=151.7817, y=-32.9283), col="red")+    # new castle
        annotate("text", x=151.7817, y=-33.0, label = "Newcastle", col="brown")+
        geom_point(aes(x=150.3557, y=-32.1393), col="red")+    # Merriwa
        annotate("text", x=150.3557, y=-32.2393, label = "Merriwa", col="brown")+
        geom_point(aes(x=151.3624, y=-32.8345), col="red")+    # Cessnock
        annotate("text", x=151.3624, y=-32.9345, label = "Cessnock", col="brown")+
        geom_point(aes(x=150.75, y=-33.6), col="red")+    # Richmond
        annotate("text", x=150.75, y=-33.7, label = "Richmond", col="brown")+
        geom_point(aes(x=151.1788, y=-32.5695), col="red")+    # Singleton
        annotate("text", x=151.1788, y=-32.6695, label = "Singleton", col="brown")+
        geom_point(aes(x=151.3417, y=-33.4267), col="red")+    # Gosford
        annotate("text", x=151.3417, y=-33.5267, label = "Gosford", col="brown")+
        annotate("text", x=150.9190, y=-33.0482, label = "Yengo NF")+
        annotate("text", x=150.3765, y=-33.0174, label = "Wollemi NF")+
        annotate("text", x=151.6632, y=-32.0671, label = "Barrington Tops NF")+
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
        scale_fill_manual(name="percentile",
                          limits=c("99.9", "99", "95", "90", "80", "70", "60", "50", "40"),
                          values=rain.color,
                          labels=c("99.9", "99", "95", "90", "80", "70", "60", "50", "40"))+
        guides(color = guide_legend(nrow=5, byrow = T))+
        ggtitle(paste0("Max. wind severity percentile"))+
        xlim(lon.min.syd, lon.max.syd)+
        ylim(lat.min.syd, lat.max.syd)
    
    ### wind speed intensity (m/s)
    p6 <- ggplot(aus.poly) +
        geom_point(wind.severity, mapping=aes(lon, lat, fill=selected.wind.speed*0.51), 
                   pch = 21, size = 5)+
        geom_sf(fill=NA) +
        geom_point(aes(x=151.2093, y=-33.8688), col="red")+  # sydney
        annotate("text", x=151.2093, y=-33.9688, label = "Sydney", col="brown")+
        geom_point(aes(x=150.8595, y=-32.0546), col="red")+    # canberra
        annotate("text", x=150.8595, y=-32.1546, label = "Scone", col="brown")+
        geom_point(aes(x=151.7817, y=-32.9283), col="red")+    # new castle
        annotate("text", x=151.7817, y=-33.0, label = "Newcastle", col="brown")+
        geom_point(aes(x=150.3557, y=-32.1393), col="red")+    # Merriwa
        annotate("text", x=150.3557, y=-32.2393, label = "Merriwa", col="brown")+
        geom_point(aes(x=151.3624, y=-32.8345), col="red")+    # Cessnock
        annotate("text", x=151.3624, y=-32.9345, label = "Cessnock", col="brown")+
        geom_point(aes(x=150.75, y=-33.6), col="red")+    # Richmond
        annotate("text", x=150.75, y=-33.7, label = "Richmond", col="brown")+
        geom_point(aes(x=151.1788, y=-32.5695), col="red")+    # Singleton
        annotate("text", x=151.1788, y=-32.6695, label = "Singleton", col="brown")+
        geom_point(aes(x=151.3417, y=-33.4267), col="red")+    # Gosford
        annotate("text", x=151.3417, y=-33.5267, label = "Gosford", col="brown")+
        annotate("text", x=150.9190, y=-33.0482, label = "Yengo NF")+
        annotate("text", x=150.3765, y=-33.0174, label = "Wollemi NF")+
        annotate("text", x=151.6632, y=-32.0671, label = "Barrington Tops NF")+
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
        guides(color = guide_legend(nrow=5, byrow = T))+
        ggtitle(paste0("Max. wind speed (m/s)"))+
        xlim(lon.min.syd, lon.max.syd)+
        ylim(lat.min.syd, lat.max.syd)
    
    ### save plot
    jpeg(paste0(destDir, "/Sydney_Hunter_Valley_", date.of.interest,
               "_storm_", storm.duration,
               "_drought_", drought.duration, ".jpg"), units="in", res=150,width = 16, height=20)
    
    
    grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 3)
    
    dev.off()
    
}
