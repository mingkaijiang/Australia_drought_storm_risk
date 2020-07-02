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
    
    ### antecedent water deficit intensity and severity
    deficit.1yr <- readRDS(paste0(sourceDir, 
                                   "/antecedent_water_deficit/antecedent_water_deficit_severity_and_intensity_", 
                                   date.of.interest, "_1-year_", user.region.name,
                                   "_regions.rds"))
    
    deficit.2yr <- readRDS(paste0(sourceDir, 
                                  "/antecedent_water_deficit/antecedent_water_deficit_severity_and_intensity_", 
                                  date.of.interest, "_2-year_", user.region.name,
                                  "_regions.rds"))
    
    
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
    #dryness.intensity.1yr.long$value <- dryness.intensity.1yr.long$value / 1000
    #dryness.intensity.2yr.long$value <- dryness.intensity.2yr.long$value / 1000
    
    
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
    
    
    deficit.1yr <- merge(deficit.1yr, ausDF, by=c("lon", "lat"), all=T)
    deficit.1yr <- subset(deficit.1yr, layer == 1)
    deficit.1yr <- subset(deficit.1yr, intensity != "NA")
    
    deficit.2yr <- merge(deficit.2yr, ausDF, by=c("lon", "lat"), all=T)
    deficit.2yr <- subset(deficit.2yr, layer == 1)
    deficit.2yr <- subset(deficit.2yr, intensity != "NA")
    
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
    
    
    
    List3 <- convert_continuous_to_discrete_bins(inDF=dryness.intensity.1yr.long, 
                                                 n.discrete.colors=n.discrete.colors)
    dryness.intensity.1yr.long <- List3$outDF
    dryness.intensity.1yr.long.lab <- List3$lab
    n.discrete.colors.dryness.intensity.1yr <- length(dryness.intensity.1yr.long.lab)
    heat.color.dryness.1yr <- rev(brewer.pal(n = n.discrete.colors.dryness.intensity.1yr, name = "YlOrRd"))
    
    List3 <- convert_continuous_to_discrete_bins(inDF=dryness.intensity.2yr.long, 
                                                 n.discrete.colors=n.discrete.colors)
    dryness.intensity.2yr.long <- List3$outDF
    dryness.intensity.2yr.long.lab <- List3$lab
    n.discrete.colors.dryness.intensity.2yr <- length(dryness.intensity.2yr.long.lab)
    heat.color.dryness.2yr <- rev(brewer.pal(n = n.discrete.colors.dryness.intensity.2yr, name = "YlOrRd"))
    
    
    
    List4 <- convert_continuous_to_discrete_bins_for_deficit(inDF=deficit.1yr, 
                                                             n.discrete.colors=n.discrete.colors)
    deficit.1yr <- List4$outDF
    deficit.1yr.lab <- List4$lab
    n.discrete.colors.deficit.intensity.1yr <- length(deficit.1yr.lab)
    heat.color.deficit.1yr <- rev(brewer.pal(n = n.discrete.colors.deficit.intensity.1yr, name = "YlOrRd"))
    
    
    List4 <- convert_continuous_to_discrete_bins_for_deficit(inDF=deficit.2yr, 
                                                             n.discrete.colors=n.discrete.colors)
    deficit.2yr <- List4$outDF
    deficit.2yr.lab <- List4$lab
    n.discrete.colors.deficit.intensity.2yr <- length(deficit.2yr.lab)
    heat.color.deficit.2yr <- rev(brewer.pal(n = n.discrete.colors.deficit.intensity.2yr, name = "YlOrRd"))
    
    
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
    ############################### 1-day rainfall ####################################
    #### ploting storm severity
    p1 <- ggplot() +
        geom_raster(storm.severity.1day.long, mapping=aes(lon, lat, fill=as.character(value)))+
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
        scale_fill_manual(name="return interval",
                          limits=c("100", "50", "20", "10", "5", "3.3", "2.5", "2", "1.67"),
                          values=rain.color,
                          labels=c("100", "50", "20", "10", "5", "3.3", "2.5", "2", "<=1.67"))+
        ggtitle(expression("1-day storm return frequency ("* yr^-1 * ")"))+
        guides(color = guide_legend(nrow=5, byrow = T))+
        xlim(user.lon.min, user.lon.max)+
        ylim(user.lat.min, user.lat.max)
    

    ### plot storm intensity (mm/d)
    p2 <- ggplot() +
        geom_tile(storm.intensity.1day.long, mapping=aes(lon, lat, fill=value_cat))+
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
        ggtitle(paste0("1-day storm intensity (mm/d)"))+
        scale_fill_manual(name="value",
                          values=rev(rain.color.storm.1day),
                          labels=storm.intensity.1day.long.lab)+
        guides(color = guide_legend(nrow=5, byrow = T))+
        xlim(user.lon.min, user.lon.max)+
        ylim(user.lat.min, user.lat.max)
    
    
    
    p1.syd <- ggplot() +
        geom_raster(storm.severity.1day.long, mapping=aes(lon, lat, fill=as.character(value)))+
        geom_polygon(data = simp2, 
                     aes(x = long, y = lat, group = group), 
                     colour = "black", fill = NA)+
        geom_sf(fill=NA) +
        #geom_point(aes(x=151.1664, y=-33.8148), col="red")+  # Lane Cove
        #annotate("text", x=151.1664, y=-33.8148, label = "Lane Cove")+
        geom_point(aes(x=151.0987, y=-33.7046), col="red")+    # Hornsby 
        annotate("text", x=151.0987, y=-33.6846, label = "Hornsby Shire")+
        geom_point(aes(x=151.152, y=-33.7416), col="red")+    # Ku-ring-gai
        annotate("text", x=151.152, y=-33.7516, label = "Ku-ring-gai")+
        geom_point(aes(x=151.2569, y=-33.676), col="red")+    # Northern Beaches 
        annotate("text", x=151.2569, y=-33.656, label = "Northern Beaches")+
        geom_point(aes(x=151.0593, y=-34.0297), col="red")+    # Sutherland 
        annotate("text", x=151.0593, y=-34.0397, label = "Sutherland Shire")+
        #geom_point(aes(x=151.1994, y=-33.8071), col="red")+    # Willoughby
        #annotate("text", x=151.1994, y=-33.8071, label = "Willoughby")+
        #geom_point(aes(x=151.1437, y=-33.8337), col="red")+    # Hunters hill
        #annotate("text", x=151.1437, y=-33.8337, label = "Hunters Hill")+
        #geom_point(aes(x=151.2422, y=-33.8865), col="red")+    # woollahra
        #annotate("text", x=151.2422, y=-33.8865, label = "Woollahra")+
        #geom_point(aes(x=151.5603, y=-33.0311), col="red")+    # Lake Macquarie
        #annotate("text", x=151.5603, y=-33.0311, label = "Lake Macquarie")+
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
        ggtitle(expression("1-day storm return frequency ("* yr^-1 * ")"))+
        guides(color = guide_legend(nrow=5, byrow = T))+
        xlim(syd.lon.min, syd.lon.max)+
        ylim(syd.lat.min, syd.lat.max)
    
    
    
    p2.syd <- ggplot() +
        geom_tile(storm.intensity.1day.long, mapping=aes(lon, lat, fill=value_cat))+
        geom_polygon(data = simp, 
                     aes(x = long, y = lat, group = group), 
                     colour = "black", fill = NA)+
        geom_sf(fill=NA) +
        #geom_point(aes(x=151.1664, y=-33.8148), col="red")+  # Lane Cove
        #annotate("text", x=151.1664, y=-33.8148, label = "Lane Cove")+
        geom_point(aes(x=151.0987, y=-33.7046), col="red")+    # Hornsby 
        annotate("text", x=151.0987, y=-33.6846, label = "Hornsby Shire")+
        geom_point(aes(x=151.152, y=-33.7416), col="red")+    # Ku-ring-gai
        annotate("text", x=151.152, y=-33.7516, label = "Ku-ring-gai")+
        geom_point(aes(x=151.2569, y=-33.676), col="red")+    # Northern Beaches 
        annotate("text", x=151.2569, y=-33.656, label = "Northern Beaches")+
        geom_point(aes(x=151.0593, y=-34.0297), col="red")+    # Sutherland 
        annotate("text", x=151.0593, y=-34.0397, label = "Sutherland Shire")+
        #geom_point(aes(x=151.1994, y=-33.8071), col="red")+    # Willoughby
        #annotate("text", x=151.1994, y=-33.8071, label = "Willoughby")+
        #geom_point(aes(x=151.1437, y=-33.8337), col="red")+    # Hunters hill
        #annotate("text", x=151.1437, y=-33.8337, label = "Hunters Hill")+
        #geom_point(aes(x=151.2422, y=-33.8865), col="red")+    # woollahra
        #annotate("text", x=151.2422, y=-33.8865, label = "Woollahra")+
        #geom_point(aes(x=151.5603, y=-33.0311), col="red")+    # Lake Macquarie
        #annotate("text", x=151.5603, y=-33.0311, label = "Lake Macquarie")+
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
        xlim(syd.lon.min, syd.lon.max)+
        ylim(syd.lat.min, syd.lat.max)
    
    
    #### save plot
    jpeg(paste0(destDir, "/Sydney_Hunter_Valley_", date.of.interest,
                "_storm_1-day.jpg"), units="in", res=150,
         width = 16, height=10)
    grid.arrange(p1, p2, nrow = 1)
    dev.off()
    
    
    ############################### 5-day rainfall ####################################
    p3 <- ggplot() +
        geom_raster(storm.severity.5day.long, mapping=aes(lon, lat, fill=as.character(value)))+
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
        scale_fill_manual(name="return interval",
                          limits=c("100", "50", "20", "10", "5", "3.3", "2.5", "2", "1.67"),
                          values=rain.color,
                          labels=c("100", "50", "20", "10", "5", "3.3", "2.5", "2", "<=1.67"))+
        ggtitle(paste0("5-day storm severity (yr1)"))+
        guides(color = guide_legend(nrow=5, byrow = T))+
        xlim(user.lon.min, user.lon.max)+
        ylim(user.lat.min, user.lat.max)
    
    ### plot storm intensity (mm/d)
    p4 <- ggplot() +
        geom_tile(storm.intensity.5day.long, mapping=aes(lon, lat, fill=value_cat))+
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
        ggtitle(paste0("Antecedent 1-year water availability severity (%)"))+
        guides(color = guide_legend(nrow=5, byrow = T))+
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
        ggtitle(paste0("Antecedent 2-year water availability severity (%)"))+
        guides(color = guide_legend(nrow=5, byrow = T))+
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
    p9 <- ggplot() +
        geom_polygon(data = simp, 
                     aes(x = long, y = lat, group = group), 
                     colour = "black", fill = NA)+
        geom_point(wind.severity, mapping=aes(lon, lat, 
                                              fill=as.character(wind.severity.percentile)), 
                   pch = 21, size = 5)+
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
                          limits=c("99.9", "99", "95", "90", "80", "70", "60", "50", "40"),
                          values=rain.color,
                          labels=c("99.9", "99", "95", "90", "80", "70", "60", "50", "40"))+
        guides(color = guide_legend(nrow=5, byrow = T))+
        ggtitle(paste0("Maximum daily wind severity percentile"))+
        xlim(user.lon.min, user.lon.max)+
        ylim(user.lat.min, user.lat.max)
    
    ### wind speed intensity (m/s)
    p10 <- ggplot() +
        geom_polygon(data = simp, 
                     aes(x = long, y = lat, group = group), 
                     colour = "black", fill = NA)+
        geom_point(wind.severity, mapping=aes(lon, lat, fill=selected.wind.speed*0.51), 
                   pch = 21, size = 5)+
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
    p11 <- ggplot() +
        geom_tile(dryness.severity.1yr.long, mapping=aes(lon, lat, fill=as.character(value)))+
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
                          limits=c("99.9", "99", "95", "90", "80", "70", "60", "50", "40"),
                          values=heat.color,
                          labels=c("99.9", "99", "95", "90", "80", "70", "60", "50", "40"))+
        ggtitle(paste0("Antecedent 1-year atmospheric dryness severity (%)"))+
        guides(color = guide_legend(nrow=5, byrow = T))+
        xlim(user.lon.min, user.lon.max)+
        ylim(user.lat.min, user.lat.max)
    
    
    ### plot dryness intensity (mm/yr)
    p12 <- ggplot() +
        geom_tile(dryness.intensity.1yr.long, mapping=aes(lon, lat, fill=value_cat))+
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
    p13 <- ggplot() +
        geom_tile(dryness.severity.2yr.long, mapping=aes(lon, lat, fill=as.character(value)))+
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
                          limits=c("99.9", "99", "95", "90", "80", "70", "60", "50", "40"),
                          values=heat.color,
                          labels=c("99.9", "99", "95", "90", "80", "70", "60", "50", "40"))+
        ggtitle(paste0("Antecedent 2-year atmospheric dryness severity (%)"))+
        guides(color = guide_legend(nrow=5, byrow = T))+
        xlim(user.lon.min, user.lon.max)+
        ylim(user.lat.min, user.lat.max)
    
    
    ### plot dryness intensity (mm/yr)
    p14 <- ggplot() +
        geom_tile(dryness.intensity.2yr.long, mapping=aes(lon, lat, fill=value_cat))+
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
                          values=rev(heat.color.dryness.2yr),
                          labels=dryness.intensity.2yr.long.lab)+
        ggtitle(paste0("Antecedent 2-year atmospheric dryness (kPa)"))+
        guides(color = guide_legend(nrow=5, byrow = T))+
        xlim(user.lon.min, user.lon.max)+
        ylim(user.lat.min, user.lat.max)
    
    
    #### save plot
    jpeg(paste0(destDir, "/Sydney_Hunter_Valley_", date.of.interest,
                "_antecedent_atmospheric_dryness_2-year.jpg"), units="in", res=150,
         width = 16, height=10)
    grid.arrange(p13, p14, nrow = 1)
    dev.off()
    
    ######################## antecedent 1-year water deficit ######################
    p15 <- ggplot() +
        geom_tile(deficit.1yr, mapping=aes(lon, lat, fill=as.character(severity)))+
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
                          limits=c("99.9", "99", "95", "90", "80", "70", "60", "50", "40"),
                          values=heat.color,
                          labels=c("99.9", "99", "95", "90", "80", "70", "60", "50", "40"))+
        ggtitle(paste0("Antecedent 1-year water deficit severity (%)"))+
        guides(color = guide_legend(nrow=5, byrow = T))+
        xlim(user.lon.min, user.lon.max)+
        ylim(user.lat.min, user.lat.max)
    
    
    ### plot dryness intensity (mm/yr)
    p16 <- ggplot() +
        geom_tile(deficit.1yr, mapping=aes(lon, lat, fill=value_cat))+
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
                          values=heat.color.deficit.1yr,
                          labels=deficit.1yr.lab)+
        ggtitle(paste0("Antecedent 1-year water deficit (P - PET, unit: mm/yr)"))+
        guides(color = guide_legend(nrow=5, byrow = T))+
        xlim(user.lon.min, user.lon.max)+
        ylim(user.lat.min, user.lat.max)
    
    
    #### save plot
    jpeg(paste0(destDir, "/Sydney_Hunter_Valley_", date.of.interest,
                "_antecedent_water_deficit_1-year.jpg"), units="in", res=150,
         width = 16, height=10)
    grid.arrange(p15, p16, nrow = 1)
    dev.off()
    
    
    ######################## antecedent 2-year water deficit ######################
    p17 <- ggplot() +
        geom_tile(deficit.2yr, mapping=aes(lon, lat, fill=as.character(severity)))+
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
                          limits=c("99.9", "99", "95", "90", "80", "70", "60", "50", "40"),
                          values=heat.color,
                          labels=c("99.9", "99", "95", "90", "80", "70", "60", "50", "40"))+
        ggtitle(paste0("Antecedent 2-year water deficit severity (%)"))+
        guides(color = guide_legend(nrow=5, byrow = T))+
        xlim(user.lon.min, user.lon.max)+
        ylim(user.lat.min, user.lat.max)
    
    
    ### plot dryness intensity (mm/yr)
    p18 <- ggplot() +
        geom_tile(deficit.2yr, mapping=aes(lon, lat, fill=value_cat))+
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
                          values=heat.color.deficit.2yr,
                          labels=deficit.2yr.lab)+
        ggtitle(paste0("Antecedent 2-year water deficit (P - PET, unit: mm/yr)"))+
        guides(color = guide_legend(nrow=5, byrow = T))+
        xlim(user.lon.min, user.lon.max)+
        ylim(user.lat.min, user.lat.max)
    
    
    #### save plot
    jpeg(paste0(destDir, "/Sydney_Hunter_Valley_", date.of.interest,
                "_antecedent_water_deficit_2-year.jpg"), units="in", res=150,
         width = 16, height=10)
    grid.arrange(p17, p18, nrow = 1)
    dev.off()
    
}
