make_spatial_plots_for_Sydney_Hunter_valley_regions_20200131 <- function(sourceDir, 
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
    ## antecedent water availability
    drought.intensity.1yr <- readRDS(paste0(sourceDir, "/antecedent_water_availability", 
                                            "/antecedent_water_availability_intensity_", 
                                            date.of.interest, "_1-year_",
                                            user.region.name, "_regions.rds"))
    
    drought.intensity.2yr <- readRDS(paste0(sourceDir, "/antecedent_water_availability", 
                                            "/antecedent_water_availability_intensity_", 
                                            date.of.interest, "_2-year_",
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
    drought.intensity.1yr.long <- melt(drought.intensity.1yr)
    colnames(drought.intensity.1yr.long) <- c("latID", "lonID", "value")
    drought.intensity.1yr.long <- merge(drought.intensity.1yr.long, latlonDF.sub,
                                        by=c("latID", "lonID"))
    
    drought.intensity.2yr.long <- melt(drought.intensity.2yr)
    colnames(drought.intensity.2yr.long) <- c("latID", "lonID", "value")
    drought.intensity.2yr.long <- merge(drought.intensity.2yr.long, latlonDF.sub,
                                        by=c("latID", "lonID"))

    drought.severity.1yr.long <- melt(drought.severity.1yr)
    colnames(drought.severity.1yr.long) <- c("latID", "lonID", "value")
    drought.severity.1yr.long <- merge(drought.severity.1yr.long, latlonDF.sub,
                                       by=c("latID", "lonID"))
    
    drought.severity.2yr.long <- melt(drought.severity.2yr)
    colnames(drought.severity.2yr.long) <- c("latID", "lonID", "value")
    drought.severity.2yr.long <- merge(drought.severity.2yr.long, latlonDF.sub,
                                       by=c("latID", "lonID"))
    

    ### read in Australia
    aus <- read_Australia_polygon()
    DF1 <- latlonDF[,c("lon", "lat")]
    ausDF <- cbind(DF1, extract(aus, DF1, df=T))
    
    ### merge australia raster and input DF and then remove sea surface
    drought.severity.1yr.long <- merge(drought.severity.1yr.long, ausDF, by=c("lon", "lat"), all=T)
    drought.severity.1yr.long <- subset(drought.severity.1yr.long, layer == 1)
    drought.severity.1yr.long <- subset(drought.severity.1yr.long, value != "NA")
    

    drought.severity.2yr.long <- merge(drought.severity.2yr.long, ausDF, by=c("lon", "lat"), all=T)
    drought.severity.2yr.long <- subset(drought.severity.2yr.long, layer == 1)
    drought.severity.2yr.long <- subset(drought.severity.2yr.long, value != "NA")
    

    drought.intensity.1yr.long <- merge(drought.intensity.1yr.long, ausDF, by=c("lon", "lat"), all=T)
    drought.intensity.1yr.long <- subset(drought.intensity.1yr.long, layer == 1)
    drought.intensity.1yr.long <- subset(drought.intensity.1yr.long, value != "NA")
    
    
    drought.intensity.2yr.long <- merge(drought.intensity.2yr.long, ausDF, by=c("lon", "lat"), all=T)
    drought.intensity.2yr.long <- subset(drought.intensity.2yr.long, layer == 1)
    drought.intensity.2yr.long <- subset(drought.intensity.2yr.long, value != "NA")
    
    #################################### prepare plot input ######################################    
    ### prepare color palette
    n.discrete.colors <- 9
    heat.color <- rev(brewer.pal(n = n.discrete.colors, name = "YlOrRd"))
    rain.color <- rev(brewer.pal(n = n.discrete.colors, name = "Blues"))
    
    ### make categorical bins for the rainfall intensity datasets
    
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
    
    
    
    ### australia polygon
    aus.poly <- ne_countries(scale = "medium", country = "Australia", returnclass = "sf")
    
    shp <- readOGR(dsn = file.path("input/NSW_LGA_POLYGON_shp.shp"), stringsAsFactors = F)
    
    e <- extent(user.lon.min, user.lon.max,
                user.lat.min, user.lat.max)
    
    crp <- crop(shp,e)
    
    simp <- gSimplify(crp, 
                      tol = 0.01, 
                      topologyPreserve = TRUE)
    
    
    ### create smaller sydney region extent
    syd.lon.min <- 150.8
    syd.lon.max <- 151.4
    syd.lat.min <- -34.2
    syd.lat.max <- -33.4
    
    e2 <- extent(syd.lon.min, syd.lon.max,
                 syd.lat.min, syd.lat.max)
    
    crp2 <- crop(shp, e2)
    
    simp2 <- gSimplify(crp2, 
                      tol = 0.001, 
                      topologyPreserve = TRUE)
    
    
    #################################### plotting ######################################    
    ### >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>           <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< ###
    ### >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>           <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< ###
    ### >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>           <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< ###
    
    
    #################### antecedent 1-year water availability ########################
    #### ploting drought severity
    p5 <- ggplot() +
        geom_tile(drought.severity.1yr.long, mapping=aes(lon, lat, fill=as.character(value)))+
        geom_polygon(data = simp, 
                     aes(x = long, y = lat, group = group), 
                     colour = "black", fill = NA)+
        geom_sf(fill=NA) +
        geom_point(aes(x=151.2093, y=-33.8688), col="red")+  # sydney
        annotate("text", x=151.5093, y=-33.9688, label = "Sydney")+
        geom_point(aes(x=152.9, y=-31.4333), col="red")+    # Port Macquarie
        annotate("text", x=152.6, y=-31.5333, label = "Port Macquarie")+
        geom_point(aes(x=151.7817, y=-32.9283), col="red")+    # new castle
        annotate("text", x=151.7817, y=-33.0, label = "Newcastle")+
        geom_point(aes(x=150.3557, y=-32.1393), col="red")+    # Merriwa
        annotate("text", x=150.3557, y=-32.2393, label = "Merriwa")+
        geom_point(aes(x=150.8931, y=-34.4278), col="red")+    # Wollongong
        annotate("text", x=150.8931, y=-34.4478, label = "Wollongong")+
        geom_point(aes(x=150.3119, y=-33.7125), col="red")+    # Katoomba
        annotate("text", x=150.3119, y=-33.8125, label = "Katoomba")+
        geom_point(aes(x=151.1788, y=-32.5695), col="red")+    # Singleton
        annotate("text", x=151.1788, y=-32.6695, label = "Singleton")+
        geom_point(aes(x=151.3417, y=-33.4267), col="red")+    # Gosford
        annotate("text", x=151.3417, y=-33.5267, label = "Gosford")+
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
        ggtitle(paste0("Antecedent 1-year water availability percentile (%)"))+
        guides(fill = guide_legend(nrow=2, byrow = T))+
        xlim(user.lon.min, user.lon.max)+
        ylim(user.lat.min, user.lat.max)
    
    
    ### plot drought intensity (mm/yr)
    p6 <- ggplot() +
        geom_tile(drought.intensity.1yr.long, mapping=aes(lon, lat, fill=value_cat))+
        geom_polygon(data = simp, 
                     aes(x = long, y = lat, group = group), 
                     colour = "black", fill = NA)+
        geom_sf(fill=NA) +
        geom_point(aes(x=151.2093, y=-33.8688), col="red")+  # sydney
        annotate("text", x=151.5093, y=-33.9688, label = "Sydney")+
        geom_point(aes(x=152.9, y=-31.4333), col="red")+    # Port Macquarie
        annotate("text", x=152.6, y=-31.5333, label = "Port Macquarie")+
        geom_point(aes(x=151.7817, y=-32.9283), col="red")+    # new castle
        annotate("text", x=151.7817, y=-33.0, label = "Newcastle")+
        geom_point(aes(x=150.3557, y=-32.1393), col="red")+    # Merriwa
        annotate("text", x=150.3557, y=-32.2393, label = "Merriwa")+
        geom_point(aes(x=150.8931, y=-34.4278), col="red")+    # Wollongong
        annotate("text", x=150.8931, y=-34.4478, label = "Wollongong")+
        geom_point(aes(x=150.3119, y=-33.7125), col="red")+    # Katoomba
        annotate("text", x=150.3119, y=-33.8125, label = "Katoomba")+
        geom_point(aes(x=151.1788, y=-32.5695), col="red")+    # Singleton
        annotate("text", x=151.1788, y=-32.6695, label = "Singleton")+
        geom_point(aes(x=151.3417, y=-33.4267), col="red")+    # Gosford
        annotate("text", x=151.3417, y=-33.5267, label = "Gosford")+
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
        ggtitle(paste0("Antecedent 1-year water availability (mm)"))+
        guides(fill = guide_legend(nrow=2, byrow = T))+
        xlim(user.lon.min, user.lon.max)+
        ylim(user.lat.min, user.lat.max)
    
    #### save plot
    jpeg(paste0(destDir, "/Sydney_Hunter_Valley_", date.of.interest,
                "_antecedent_water_availability_1-year.jpg"), units="in", res=150,
         width = 16, height=10)
    grid.arrange(p5, p6, nrow = 1)
    dev.off()
    
    
    #################### antecedent 2-year water availability ########################
    p7 <- ggplot() +
        geom_tile(drought.severity.2yr.long, mapping=aes(lon, lat, fill=as.character(value)))+
        geom_polygon(data = simp, 
                     aes(x = long, y = lat, group = group), 
                     colour = "black", fill = NA)+
        geom_sf(fill=NA) +
        geom_point(aes(x=151.2093, y=-33.8688), col="red")+  # sydney
        annotate("text", x=151.5093, y=-33.9688, label = "Sydney")+
        geom_point(aes(x=152.9, y=-31.4333), col="red")+    # Port Macquarie
        annotate("text", x=152.6, y=-31.5333, label = "Port Macquarie")+
        geom_point(aes(x=151.7817, y=-32.9283), col="red")+    # new castle
        annotate("text", x=151.7817, y=-33.0, label = "Newcastle")+
        geom_point(aes(x=150.3557, y=-32.1393), col="red")+    # Merriwa
        annotate("text", x=150.3557, y=-32.2393, label = "Merriwa")+
        geom_point(aes(x=150.8931, y=-34.4278), col="red")+    # Wollongong
        annotate("text", x=150.8931, y=-34.4478, label = "Wollongong")+
        geom_point(aes(x=150.3119, y=-33.7125), col="red")+    # Katoomba
        annotate("text", x=150.3119, y=-33.8125, label = "Katoomba")+
        geom_point(aes(x=151.1788, y=-32.5695), col="red")+    # Singleton
        annotate("text", x=151.1788, y=-32.6695, label = "Singleton")+
        geom_point(aes(x=151.3417, y=-33.4267), col="red")+    # Gosford
        annotate("text", x=151.3417, y=-33.5267, label = "Gosford")+
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
        ggtitle(paste0("Antecedent 2-year rainfall percentile (%)"))+
        guides(fill = guide_legend(nrow=2, byrow = T))+
        xlim(user.lon.min, user.lon.max)+
        ylim(user.lat.min, user.lat.max)
    
    
    ### plot drought intensity (mm/yr)
    p8 <- ggplot() +
        geom_tile(drought.intensity.2yr.long, mapping=aes(lon, lat, fill=value_cat))+
        geom_polygon(data = simp, 
                     aes(x = long, y = lat, group = group), 
                     colour = "black", fill = NA)+
        geom_sf(fill=NA) +
        geom_point(aes(x=151.2093, y=-33.8688), col="red")+  # sydney
        annotate("text", x=151.5093, y=-33.9688, label = "Sydney")+
        geom_point(aes(x=152.9, y=-31.4333), col="red")+    # Port Macquarie
        annotate("text", x=152.6, y=-31.5333, label = "Port Macquarie")+
        geom_point(aes(x=151.7817, y=-32.9283), col="red")+    # new castle
        annotate("text", x=151.7817, y=-33.0, label = "Newcastle")+
        geom_point(aes(x=150.3557, y=-32.1393), col="red")+    # Merriwa
        annotate("text", x=150.3557, y=-32.2393, label = "Merriwa")+
        geom_point(aes(x=150.8931, y=-34.4278), col="red")+    # Wollongong
        annotate("text", x=150.8931, y=-34.4478, label = "Wollongong")+
        geom_point(aes(x=150.3119, y=-33.7125), col="red")+    # Katoomba
        annotate("text", x=150.3119, y=-33.8125, label = "Katoomba")+
        geom_point(aes(x=151.1788, y=-32.5695), col="red")+    # Singleton
        annotate("text", x=151.1788, y=-32.6695, label = "Singleton")+
        geom_point(aes(x=151.3417, y=-33.4267), col="red")+    # Gosford
        annotate("text", x=151.3417, y=-33.5267, label = "Gosford")+
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
        ggtitle(paste0("Antecedent 2-year rainfall (mm)"))+
        guides(fill = guide_legend(nrow=2, byrow = T))+
        xlim(user.lon.min, user.lon.max)+
        ylim(user.lat.min, user.lat.max)
    
    
    
    #### save plot
    jpeg(paste0(destDir, "/Sydney_Hunter_Valley_", date.of.interest,
                "_antecedent_water_availability_2-year.jpg"), units="in", res=150,
         width = 16, height=10)
    grid.arrange(p7, p8, nrow = 1)
    dev.off()
    
    
    
    
    
}
