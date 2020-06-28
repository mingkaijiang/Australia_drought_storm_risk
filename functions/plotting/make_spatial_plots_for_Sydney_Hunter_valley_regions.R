make_spatial_plots_for_Sydney_Hunter_valley_regions <- function(sourceDir, 
                                                                destDir,
                                                                user.region.name,
                                                                date.of.interest,
                                                                storm.duration,
                                                                drought.duration) {

    #################################### prepare datasets ######################################    
    #### Create output folder
    if(!dir.exists(destDir)) {
        dir.create(destDir, showWarnings = FALSE)
    }
    
    #### fixed user defined lat and lon information, for Sydney and Hunter Valley regions
    user.lat.max = -31
    user.lat.min = -35
    user.lon.max = 153
    user.lon.min = 149
    
    #### read in data
    ## storm rainfall total
    storm.intensity.1day <- readRDS(paste0(sourceDir, "/storm",
                                           "/storm_intensity_", 
                                           date.of.interest, "_1-day_",
                                           user.region.name, "_regions.rds"))
    
    storm.intensity.5day <- readRDS(paste0(sourceDir, "/storm",
                                           "/storm_intensity_", 
                                           date.of.interest, "_5-day_",
                                           user.region.name, "_regions.rds"))
    
    ## antecedent water availability
    drought.intensity.1yr <- readRDS(paste0(sourceDir, "/antecedent_water_availability", 
                                            "/antecedent_water_availability_intensity_", 
                                            date.of.interest, "_1-year_",
                                            user.region.name, "_regions.rds"))
    
    drought.intensity.2yr <- readRDS(paste0(sourceDir, "/antecedent_water_availability", 
                                            "/antecedent_water_availability_intensity_", 
                                            date.of.interest, "_2-year_",
                                            user.region.name, "_regions.rds"))
    
    ## antecedent atmospheric dryness
    dryness.intensity.1yr <- readRDS(paste0(sourceDir, "/antecedent_atmospheric_dryness", 
                                            "/antecedent_atmospheric_dryness_intensity_", 
                                            date.of.interest, "_1-year_",
                                            user.region.name, "_regions.rds"))
    
    dryness.intensity.2yr <- readRDS(paste0(sourceDir, "/antecedent_atmospheric_dryness", 
                                            "/antecedent_atmospheric_dryness_intensity_", 
                                            date.of.interest, "_2-year_",
                                            user.region.name, "_regions.rds"))
    
    
    ## storm severity
    storm.severity.1day <- readRDS(paste0(sourceDir, "/storm",
                                          "/storm_return_severity_", 
                                          date.of.interest, "_1-day_",
                                          user.region.name, "_regions.rds"))
    
    storm.severity.5day <- readRDS(paste0(sourceDir, "/storm",
                                          "/storm_return_severity_", 
                                          date.of.interest, "_5-day_",
                                          user.region.name, "_regions.rds"))
    
    ## drought severity
    drought.severity.1yr <- readRDS(paste0(sourceDir, "/antecedent_water_availability", 
                                           "/antecedent_water_availability_severity_", 
                                           date.of.interest, "_1-year_",
                                           user.region.name, "_regions.rds"))
    
    drought.severity.2yr <- readRDS(paste0(sourceDir, "/antecedent_water_availability", 
                                           "/antecedent_water_availability_severity_", 
                                           date.of.interest, "_2-year_",
                                           user.region.name, "_regions.rds"))
    
    ## atmospheric dryness severity
    dryness.severity.1yr <- readRDS(paste0(sourceDir, "/antecedent_atmospheric_dryness", 
                                           "/antecedent_atmospheric_dryness_severity_", 
                                           date.of.interest, "_1-year_",
                                           user.region.name, "_regions.rds"))
    
    dryness.severity.2yr <- readRDS(paste0(sourceDir, "/antecedent_atmospheric_dryness", 
                                           "/antecedent_atmospheric_dryness_severity_", 
                                           date.of.interest, "_2-year_",
                                           user.region.name, "_regions.rds"))
    
    
    ## wind intensity and severity
    wind.severity <- read.csv(paste0(sourceDir, "/wind/GSOD_Wind_Extreme_Severity_", 
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
    storm.intensity.1day.long <- melt(storm.intensity.1day)
    colnames(storm.intensity.1day.long) <- c("latID", "lonID", "value")
    storm.intensity.1day.long <- merge(storm.intensity.1day.long, latlonDF.sub,
                                       by=c("latID", "lonID"))
    
    storm.intensity.5day.long <- melt(storm.intensity.5day)
    colnames(storm.intensity.5day.long) <- c("latID", "lonID", "value")
    storm.intensity.5day.long <- merge(storm.intensity.5day.long, latlonDF.sub,
                                       by=c("latID", "lonID"))
    
    drought.intensity.1yr.long <- melt(drought.intensity.1yr)
    colnames(drought.intensity.1yr.long) <- c("latID", "lonID", "value")
    drought.intensity.1yr.long <- merge(drought.intensity.1yr.long, latlonDF.sub,
                                        by=c("latID", "lonID"))
    
    drought.intensity.2yr.long <- melt(drought.intensity.2yr)
    colnames(drought.intensity.2yr.long) <- c("latID", "lonID", "value")
    drought.intensity.2yr.long <- merge(drought.intensity.2yr.long, latlonDF.sub,
                                        by=c("latID", "lonID"))
    
    dryness.intensity.1yr.long <- melt(dryness.intensity.1yr)
    colnames(dryness.intensity.1yr.long) <- c("latID", "lonID", "value")
    dryness.intensity.1yr.long <- merge(dryness.intensity.1yr.long, latlonDF.sub,
                                        by=c("latID", "lonID"))
    
    dryness.intensity.2yr.long <- melt(dryness.intensity.2yr)
    colnames(dryness.intensity.2yr.long) <- c("latID", "lonID", "value")
    dryness.intensity.2yr.long <- merge(dryness.intensity.2yr.long, latlonDF.sub,
                                        by=c("latID", "lonID"))
    
    ## convert unit from Pa to kPa
    dryness.intensity.1yr.long$value <- dryness.intensity.1yr.long$value / 1000
    dryness.intensity.2yr.long$value <- dryness.intensity.2yr.long$value / 1000
    
    
    storm.severity.1day.long <- melt(storm.severity.1day)
    colnames(storm.severity.1day.long) <- c("latID", "lonID", "value")
    storm.severity.1day.long <- merge(storm.severity.1day.long, latlonDF.sub,
                                      by=c("latID", "lonID"))
    
    storm.severity.5day.long <- melt(storm.severity.5day)
    colnames(storm.severity.5day.long) <- c("latID", "lonID", "value")
    storm.severity.5day.long <- merge(storm.severity.5day.long, latlonDF.sub,
                                      by=c("latID", "lonID"))
    
    drought.severity.1yr.long <- melt(drought.severity.1yr)
    colnames(drought.severity.1yr.long) <- c("latID", "lonID", "value")
    drought.severity.1yr.long <- merge(drought.severity.1yr.long, latlonDF.sub,
                                       by=c("latID", "lonID"))
    
    drought.severity.2yr.long <- melt(drought.severity.2yr)
    colnames(drought.severity.2yr.long) <- c("latID", "lonID", "value")
    drought.severity.2yr.long <- merge(drought.severity.2yr.long, latlonDF.sub,
                                       by=c("latID", "lonID"))
    
    dryness.severity.1yr.long <- melt(dryness.severity.1yr)
    colnames(dryness.severity.1yr.long) <- c("latID", "lonID", "value")
    dryness.severity.1yr.long <- merge(dryness.severity.1yr.long, latlonDF.sub,
                                       by=c("latID", "lonID"))
    
    dryness.severity.2yr.long <- melt(dryness.severity.2yr)
    colnames(dryness.severity.2yr.long) <- c("latID", "lonID", "value")
    dryness.severity.2yr.long <- merge(dryness.severity.2yr.long, latlonDF.sub,
                                       by=c("latID", "lonID"))
    
    
    ### read in Australia
    aus <- read_Australia_polygon()
    DF1 <- latlonDF[,c("lon", "lat")]
    ausDF <- cbind(DF1, extract(aus, DF1, df=T))
    
    ### merge australia raster and input DF and then remove sea surface
    drought.severity.1yr.long <- merge(drought.severity.1yr.long, ausDF, by=c("lon", "lat"), all=T)
    drought.severity.1yr.long <- subset(drought.severity.1yr.long, layer == 1)
    drought.severity.1yr.long <- subset(drought.severity.1yr.long, value != "NA")
    
    dryness.severity.1yr.long <- merge(dryness.severity.1yr.long, ausDF, by=c("lon", "lat"), all=T)
    dryness.severity.1yr.long <- subset(dryness.severity.1yr.long, layer == 1)
    dryness.severity.1yr.long <- subset(dryness.severity.1yr.long, value != "NA")
    
    drought.severity.2yr.long <- merge(drought.severity.2yr.long, ausDF, by=c("lon", "lat"), all=T)
    drought.severity.2yr.long <- subset(drought.severity.2yr.long, layer == 1)
    drought.severity.2yr.long <- subset(drought.severity.2yr.long, value != "NA")
    
    dryness.severity.2yr.long <- merge(dryness.severity.2yr.long, ausDF, by=c("lon", "lat"), all=T)
    dryness.severity.2yr.long <- subset(dryness.severity.2yr.long, layer == 1)
    dryness.severity.2yr.long <- subset(dryness.severity.2yr.long, value != "NA")
    
    storm.severity.1day.long <- merge(storm.severity.1day.long, ausDF, by=c("lon", "lat"), all=T)
    storm.severity.1day.long <- subset(storm.severity.1day.long, layer == 1)
    storm.severity.1day.long <- subset(storm.severity.1day.long, value != "NA")
    
    storm.severity.5day.long <- merge(storm.severity.5day.long, ausDF, by=c("lon", "lat"), all=T)
    storm.severity.5day.long <- subset(storm.severity.5day.long, layer == 1)
    storm.severity.5day.long <- subset(storm.severity.5day.long, value != "NA")
    
    
    
    drought.intensity.1yr.long <- merge(drought.intensity.1yr.long, ausDF, by=c("lon", "lat"), all=T)
    drought.intensity.1yr.long <- subset(drought.intensity.1yr.long, layer == 1)
    drought.intensity.1yr.long <- subset(drought.intensity.1yr.long, value != "NA")
    
    dryness.intensity.1yr.long <- merge(dryness.intensity.1yr.long, ausDF, by=c("lon", "lat"), all=T)
    dryness.intensity.1yr.long <- subset(dryness.intensity.1yr.long, layer == 1)
    dryness.intensity.1yr.long <- subset(dryness.intensity.1yr.long, value != "NA")
    
    drought.intensity.2yr.long <- merge(drought.intensity.2yr.long, ausDF, by=c("lon", "lat"), all=T)
    drought.intensity.2yr.long <- subset(drought.intensity.2yr.long, layer == 1)
    drought.intensity.2yr.long <- subset(drought.intensity.2yr.long, value != "NA")
    
    dryness.intensity.2yr.long <- merge(dryness.intensity.2yr.long, ausDF, by=c("lon", "lat"), all=T)
    dryness.intensity.2yr.long <- subset(dryness.intensity.2yr.long, layer == 1)
    dryness.intensity.2yr.long <- subset(dryness.intensity.2yr.long, value != "NA")
    
    storm.intensity.1day.long <- merge(storm.intensity.1day.long, ausDF, by=c("lon", "lat"), all=T)
    storm.intensity.1day.long <- subset(storm.intensity.1day.long, layer == 1)
    storm.intensity.1day.long <- subset(storm.intensity.1day.long, value != "NA")
    
    storm.intensity.5day.long <- merge(storm.intensity.5day.long, ausDF, by=c("lon", "lat"), all=T)
    storm.intensity.5day.long <- subset(storm.intensity.5day.long, layer == 1)
    storm.intensity.5day.long <- subset(storm.intensity.5day.long, value != "NA")
    
    
    #################################### prepare plot input ######################################    
    ### prepare color palette
    n.discrete.colors <- 9
    heat.color <- rev(brewer.pal(n = n.discrete.colors, name = "YlOrRd"))
    rain.color <- rev(brewer.pal(n = n.discrete.colors, name = "Blues"))
    
    ### make categorical bins for the rainfall intensity datasets
    List1 <- convert_continuous_to_discrete_bins(inDF=storm.intensity.1day.long, 
                                                 n.discrete.colors=n.discrete.colors)
    storm.intensity.1day.long <- List1$outDF
    storm.intensity.1day.long.lab <- List1$lab
    n.discrete.colors.storm.intensity.1day <- length(storm.intensity.1day.long.lab)
    rain.color.storm.1day <- rev(brewer.pal(n = n.discrete.colors.storm.intensity.1day, name = "Blues"))
    
    List1 <- convert_continuous_to_discrete_bins(inDF=storm.intensity.5day.long, 
                                                 n.discrete.colors=n.discrete.colors)
    storm.intensity.5day.long <- List1$outDF
    storm.intensity.5day.long.lab <- List1$lab
    n.discrete.colors.storm.intensity.5day <- length(storm.intensity.5day.long.lab)
    rain.color.storm.5day <- rev(brewer.pal(n = n.discrete.colors.storm.intensity.5day, name = "Blues"))
    
    
    List2 <- convert_continuous_to_discrete_bins(inDF=drought.intensity.1yr.long, 
                                                 n.discrete.colors=n.discrete.colors)
    drought.intensity.1yr.long <- List2$outDF
    drought.intensity.1yr.long.lab <- List2$lab
    n.discrete.colors.drought.intensity.1yr <- length(drought.intensity.1yr.long.lab)
    rain.color.drought.1yr <- rev(brewer.pal(n = n.discrete.colors.drought.intensity.1yr, name = "Blues"))
    
    List2 <- convert_continuous_to_discrete_bins(inDF=drought.intensity.2yr.long, 
                                                 n.discrete.colors=n.discrete.colors)
    drought.intensity.2yr.long <- List2$outDF
    drought.intensity.2yr.long.lab <- List2$lab
    n.discrete.colors.drought.intensity.2yr <- length(drought.intensity.2yr.long.lab)
    rain.color.drought.2yr <- rev(brewer.pal(n = n.discrete.colors.drought.intensity.2yr, name = "Blues"))
    
    
    
    List2 <- convert_continuous_to_discrete_bins(inDF=dryness.intensity.1yr.long, 
                                                 n.discrete.colors=n.discrete.colors)
    dryness.intensity.1yr.long <- List2$outDF
    dryness.intensity.1yr.long.lab <- List2$lab
    n.discrete.colors.dryness.intensity.1yr <- length(dryness.intensity.1yr.long.lab)
    heat.color.dryness.1yr <- rev(brewer.pal(n = n.discrete.colors.dryness.intensity.1yr, name = "YlOrRd"))
    
    List2 <- convert_continuous_to_discrete_bins(inDF=dryness.intensity.2yr.long, 
                                                 n.discrete.colors=n.discrete.colors)
    dryness.intensity.2yr.long <- List2$outDF
    dryness.intensity.2yr.long.lab <- List2$lab
    n.discrete.colors.dryness.intensity.2yr <- length(dryness.intensity.2yr.long.lab)
    heat.color.dryness.2yr <- rev(brewer.pal(n = n.discrete.colors.dryness.intensity.2yr, name = "YlOrRd"))
    
    ### australia polygon
    aus.poly <- ne_countries(scale = "medium", country = "Australia", returnclass = "sf")
    
    
    
    #################################### plotting ######################################    
    ### >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>           <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< ###
    ### >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>           <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< ###
    ### >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>           <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< ###
    ############################### 1-day rainfall ####################################
    #### ploting storm severity
    p1 <- ggplot(aus.poly) +
        geom_raster(storm.severity.1day.long, mapping=aes(lon, lat, fill=as.character(value)))+
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
        scale_fill_manual(name="return interval",
                          limits=c("100", "50", "20", "10", "5", "3.3", "2.5", "2", "1.67"),
                          values=rain.color,
                          labels=c("100", "50", "20", "10", "5", "3.3", "2.5", "2", "<=1.67"))+
        ggtitle(paste0("1-day storm severity (yr-1)"))+
        guides(color = guide_legend(nrow=5, byrow = T))+
        xlim(user.lon.min, user.lon.max)+
        ylim(user.lat.min, user.lat.max)
    
    ### plot storm intensity (mm/d)
    p2 <- ggplot(aus.poly) +
        geom_tile(storm.intensity.1day.long, mapping=aes(lon, lat, fill=value_cat))+
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
        ggtitle(paste0("1-day storm intensity (mm/d)"))+
        scale_fill_manual(name="value",
                          values=rev(rain.color.storm.1day),
                          labels=storm.intensity.1day.long.lab)+
        guides(color = guide_legend(nrow=5, byrow = T))+
        xlim(user.lon.min, user.lon.max)+
        ylim(user.lat.min, user.lat.max)
    
    
    #### save plot
    jpeg(paste0(destDir, "/Sydney_Hunter_Valley_", date.of.interest,
                "_storm_1-day.jpg"), units="in", res=150,
         width = 16, height=10)
    grid.arrange(p1, p2, nrow = 1)
    dev.off()
    
    
    ############################### 5-day rainfall ####################################
    p3 <- ggplot(aus.poly) +
        geom_raster(storm.severity.5day.long, mapping=aes(lon, lat, fill=as.character(value)))+
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
        scale_fill_manual(name="return interval",
                          limits=c("100", "50", "20", "10", "5", "3.3", "2.5", "2", "1.67"),
                          values=rain.color,
                          labels=c("100", "50", "20", "10", "5", "3.3", "2.5", "2", "<=1.67"))+
        ggtitle(paste0("1-day storm severity (yr1)"))+
        guides(color = guide_legend(nrow=5, byrow = T))+
        xlim(user.lon.min, user.lon.max)+
        ylim(user.lat.min, user.lat.max)
    
    ### plot storm intensity (mm/d)
    p4 <- ggplot(aus.poly) +
        geom_tile(storm.intensity.5day.long, mapping=aes(lon, lat, fill=value_cat))+
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
        ggtitle(paste0("5-day storm intensity (mm/d)"))+
        scale_fill_manual(name="value",
                          values=rev(rain.color.storm.5day),
                          labels=storm.intensity.5day.long.lab)+
        guides(color = guide_legend(nrow=5, byrow = T))+
        xlim(user.lon.min, user.lon.max)+
        ylim(user.lat.min, user.lat.max)
    
    
    #### save plot
    jpeg(paste0(destDir, "/Sydney_Hunter_Valley_", date.of.interest,
                "_storm_5-day.jpg"), units="in", res=150,
         width = 16, height=10)
    grid.arrange(p3, p4, nrow = 1)
    dev.off()
    
    
    #################### antecedent 1-year water availability ########################
    #### ploting drought severity
    p5 <- ggplot(aus.poly) +
        geom_tile(drought.severity.1yr.long, mapping=aes(lon, lat, fill=as.character(value)))+
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
                          labels=c("0.1", "1", "5", "10", "20", "30", "40", "50", ">60"))+
        ggtitle(paste0("Antecedent 1-year water availability severity (%)"))+
        guides(color = guide_legend(nrow=5, byrow = T))+
        xlim(user.lon.min, user.lon.max)+
        ylim(user.lat.min, user.lat.max)
    
    
    ### plot drought intensity (mm/yr)
    p6 <- ggplot(aus.poly) +
        geom_tile(drought.intensity.1yr.long, mapping=aes(lon, lat, fill=value_cat))+
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
                          values=rev(rain.color.drought.1yr),
                          labels=drought.intensity.1yr.long.lab)+
        ggtitle(paste0("Antecedent 1-year water availability (mm/yr)"))+
        guides(color = guide_legend(nrow=5, byrow = T))+
        xlim(user.lon.min, user.lon.max)+
        ylim(user.lat.min, user.lat.max)
    
    #### save plot
    jpeg(paste0(destDir, "/Sydney_Hunter_Valley_", date.of.interest,
                "_antecedent_water_availability_1-year.jpg"), units="in", res=150,
         width = 16, height=10)
    grid.arrange(p5, p6, nrow = 1)
    dev.off()
    
    
    #################### antecedent 2-year water availability ########################
    p7 <- ggplot(aus.poly) +
        geom_tile(drought.severity.2yr.long, mapping=aes(lon, lat, fill=as.character(value)))+
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
                          labels=c("0.1", "1", "5", "10", "20", "30", "40", "50", ">60"))+
        ggtitle(paste0("Antecedent 2-year water availability severity (%)"))+
        guides(color = guide_legend(nrow=5, byrow = T))+
        xlim(user.lon.min, user.lon.max)+
        ylim(user.lat.min, user.lat.max)
    
    
    ### plot drought intensity (mm/yr)
    p8 <- ggplot(aus.poly) +
        geom_tile(drought.intensity.2yr.long, mapping=aes(lon, lat, fill=value_cat))+
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
                          values=rev(rain.color.drought.2yr),
                          labels=drought.intensity.2yr.long.lab)+
        ggtitle(paste0("Antecedent 2-year water availability (mm/yr)"))+
        guides(color = guide_legend(nrow=5, byrow = T))+
        xlim(user.lon.min, user.lon.max)+
        ylim(user.lat.min, user.lat.max)
    
    
    
    #### save plot
    jpeg(paste0(destDir, "/Sydney_Hunter_Valley_", date.of.interest,
                "_antecedent_water_availability_2-year.jpg"), units="in", res=150,
         width = 16, height=10)
    grid.arrange(p7, p8, nrow = 1)
    dev.off()
    
    
    
    ################################### wind speed ####################################
    ### wind severity percentile
    p9 <- ggplot(aus.poly) +
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
        ggtitle(paste0("Maximum daily wind severity percentile"))+
        xlim(user.lon.min, user.lon.max)+
        ylim(user.lat.min, user.lat.max)
    
    ### wind speed intensity (m/s)
    p10 <- ggplot(aus.poly) +
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
        ggtitle(paste0("Maximum daily wind speed (m/s)"))+
        xlim(user.lon.min, user.lon.max)+
        ylim(user.lat.min, user.lat.max)
    
    
    #### save plot
    jpeg(paste0(destDir, "/Sydney_Hunter_Valley_", date.of.interest,
                "_max_wind_speed.jpg"), units="in", res=150,
         width = 16, height=10)
    grid.arrange(p9, p10, nrow = 1)
    dev.off()
    
    
    ######################## antecedent 1-year atmospheric dryness ######################
    p11 <- ggplot(aus.poly) +
        geom_tile(dryness.severity.1yr.long, mapping=aes(lon, lat, fill=as.character(value)))+
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
                          values=heat.color,
                          labels=c("99.9", "99", "95", "90", "80", "70", "60", "50", "40"))+
        ggtitle(paste0("Antecedent 1-year atmospheric dryness severity (%)"))+
        guides(color = guide_legend(nrow=5, byrow = T))+
        xlim(user.lon.min, user.lon.max)+
        ylim(user.lat.min, user.lat.max)
    
    
    ### plot dryness intensity (mm/yr)
    p12 <- ggplot(aus.poly) +
        geom_tile(dryness.intensity.1yr.long, mapping=aes(lon, lat, fill=value_cat))+
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
                          values=rev(heat.color.dryness.1yr),
                          labels=dryness.intensity.1yr.long.lab)+
        ggtitle(paste0("Antecedent 1-year atmospheric dryness (kPa)"))+
        guides(color = guide_legend(nrow=5, byrow = T))+
        xlim(user.lon.min, user.lon.max)+
        ylim(user.lat.min, user.lat.max)
    
    #### save plot
    jpeg(paste0(destDir, "/Sydney_Hunter_Valley_", date.of.interest,
                "_antecedent_atmospheric_dryness_1-year.jpg"), units="in", res=150,
         width = 16, height=10)
    grid.arrange(p11, p12, nrow = 1)
    dev.off()
    
    ######################## antecedent 2-year atmospheric dryness ######################
    #### place holder
    
    #### place holder
    
    
    #### save plot
    jpeg(paste0(destDir, "/Sydney_Hunter_Valley_", date.of.interest,
                "_antecedent_atmospheric_dryness_2-year.jpg"), units="in", res=150,
         width = 16, height=10)
    grid.arrange(p13, p14, nrow = 1)
    dev.off()
    
    ######################## antecedent 1-year water deficit ######################
    #### place holder
    
    
    #### place holder
    
    
    #### save plot
    jpeg(paste0(destDir, "/Sydney_Hunter_Valley_", date.of.interest,
                "_antecedent_water_deficit_1-year.jpg"), units="in", res=150,
         width = 16, height=10)
    grid.arrange(p15, p16, nrow = 1)
    dev.off()
    
    
    ######################## antecedent 2-year water deficit ######################
    #### place holder
    
    
    #### place holder
    
    
    #### save plot
    jpeg(paste0(destDir, "/Sydney_Hunter_Valley_", date.of.interest,
                "_antecedent_water_deficit_2-year.jpg"), units="in", res=150,
         width = 16, height=10)
    grid.arrange(p17, p18, nrow = 1)
    dev.off()
    
}
