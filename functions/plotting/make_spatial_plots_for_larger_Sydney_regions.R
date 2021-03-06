make_spatial_plots_for_larger_Sydney_regions <- function(sourceDir, destDir,
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
        geom_tile(storm.severity.long, mapping=aes(lon, lat, fill=as.character(value)))+
        geom_sf(fill=NA) +
        geom_point(aes(x=151.2093, y=-33.8688), col="red")+  # sydney
        annotate("text", x=151.2093, y=-34.2, label = "Sydney")+
        geom_point(aes(x=149.13, y=-35.2809), col="red")+    # canberra
        annotate("text", x=149.13, y=-35.5, label = "Canberra")+
        geom_point(aes(x=151.7817, y=-32.9283), col="red")+    # new castle
        annotate("text", x=151.7817, y=-33.1, label = "Newcastle")+
        geom_point(aes(x=149.5775, y=-33.4193), col="red")+    # Bathurst
        annotate("text", x=149.5775, y=-33.8, label = "Bathurst")+
        geom_point(aes(x=147.3598, y=-35.1082), col="red")+    # Wagga Wagga
        annotate("text", x=147.3598, y=-35.5, label = "Wagga Wagga")+
        geom_point(aes(x=149.7812, y=-30.3324), col="red")+    # Narrabri
        annotate("text", x=149.7812, y=-30.8, label = "Narrabri")+
        geom_point(aes(x=152.9, y=-31.4333), col="red")+    # Port Macquarie
        annotate("text", x=152.9, y=-31.9, label = "Port Macquarie")+
        geom_point(aes(x=145.9378, y=-30.0888), col="red")+    # Bourke
        annotate("text", x=145.9378, y=-30.5, label = "Bourke")+
        geom_point(aes(x=153.4, y=-28.0167), col="red")+    # Gold Coast
        annotate("text", x=153.4, y=-28.5, label = "Gold Coast")+
        geom_point(aes(x=146.0455, y=-34.2801), col="red")+    # Griffith
        annotate("text", x=146.0455, y=-34.6, label = "Griffith")+
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
        xlim(user.lon.min, user.lon.max)+
        ylim(user.lat.min, user.lat.max)
    
    ### plot storm intensity
    p2 <- ggplot(aus.poly) +
        geom_tile(storm.intensity.long, mapping=aes(lon, lat, fill=value_cat))+
        geom_sf(fill=NA) +
        geom_point(aes(x=151.2093, y=-33.8688), col="red")+  # sydney
        annotate("text", x=151.2093, y=-34.2, label = "Sydney")+
        geom_point(aes(x=149.13, y=-35.2809), col="red")+    # canberra
        annotate("text", x=149.13, y=-35.5, label = "Canberra")+
        geom_point(aes(x=151.7817, y=-32.9283), col="red")+    # new castle
        annotate("text", x=151.7817, y=-33.1, label = "Newcastle")+
        geom_point(aes(x=149.5775, y=-33.4193), col="red")+    # Bathurst
        annotate("text", x=149.5775, y=-33.8, label = "Bathurst")+
        geom_point(aes(x=147.3598, y=-35.1082), col="red")+    # Wagga Wagga
        annotate("text", x=147.3598, y=-35.5, label = "Wagga Wagga")+
        geom_point(aes(x=149.7812, y=-30.3324), col="red")+    # Narrabri
        annotate("text", x=149.7812, y=-30.8, label = "Narrabri")+
        geom_point(aes(x=152.9, y=-31.4333), col="red")+    # Port Macquarie
        annotate("text", x=152.9, y=-31.9, label = "Port Macquarie")+
        geom_point(aes(x=145.9378, y=-30.0888), col="red")+    # Bourke
        annotate("text", x=145.9378, y=-30.5, label = "Bourke")+
        geom_point(aes(x=153.4, y=-28.0167), col="red")+    # Gold Coast
        annotate("text", x=153.4, y=-28.5, label = "Gold Coast")+
        geom_point(aes(x=146.0455, y=-34.2801), col="red")+    # Griffith
        annotate("text", x=146.0455, y=-34.6, label = "Griffith")+
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
        guides(color = guide_legend(nrow=5, byrow = T))+
        xlim(user.lon.min, user.lon.max)+
        ylim(user.lat.min, user.lat.max)
    
    #### ploting drought severity
    p3 <- ggplot(aus.poly) +
        geom_tile(drought.severity.long, mapping=aes(lon, lat, fill=as.character(value)))+
        geom_sf(fill=NA) +
        geom_point(aes(x=151.2093, y=-33.8688), col="red")+  # sydney
        annotate("text", x=151.2093, y=-34.2, label = "Sydney")+
        geom_point(aes(x=149.13, y=-35.2809), col="red")+    # canberra
        annotate("text", x=149.13, y=-35.5, label = "Canberra")+
        geom_point(aes(x=151.7817, y=-32.9283), col="red")+    # new castle
        annotate("text", x=151.7817, y=-33.1, label = "Newcastle")+
        geom_point(aes(x=149.5775, y=-33.4193), col="red")+    # Bathurst
        annotate("text", x=149.5775, y=-33.8, label = "Bathurst")+
        geom_point(aes(x=147.3598, y=-35.1082), col="red")+    # Wagga Wagga
        annotate("text", x=147.3598, y=-35.5, label = "Wagga Wagga")+
        geom_point(aes(x=149.7812, y=-30.3324), col="red")+    # Narrabri
        annotate("text", x=149.7812, y=-30.8, label = "Narrabri")+
        geom_point(aes(x=152.9, y=-31.4333), col="red")+    # Port Macquarie
        annotate("text", x=152.9, y=-31.9, label = "Port Macquarie")+
        geom_point(aes(x=145.9378, y=-30.0888), col="red")+    # Bourke
        annotate("text", x=145.9378, y=-30.5, label = "Bourke")+
        geom_point(aes(x=153.4, y=-28.0167), col="red")+    # Gold Coast
        annotate("text", x=153.4, y=-28.5, label = "Gold Coast")+
        geom_point(aes(x=146.0455, y=-34.2801), col="red")+    # Griffith
        annotate("text", x=146.0455, y=-34.6, label = "Griffith")+
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
        xlim(user.lon.min, user.lon.max)+
        ylim(user.lat.min, user.lat.max)
    
    
    ### plot drought intensity
    p4 <- ggplot(aus.poly) +
        geom_tile(drought.intensity.long, mapping=aes(lon, lat, fill=value_cat))+
        geom_sf(fill=NA) +
        geom_point(aes(x=151.2093, y=-33.8688), col="red")+  # sydney
        annotate("text", x=151.2093, y=-34.2, label = "Sydney")+
        geom_point(aes(x=149.13, y=-35.2809), col="red")+    # canberra
        annotate("text", x=149.13, y=-35.5, label = "Canberra")+
        geom_point(aes(x=151.7817, y=-32.9283), col="red")+    # new castle
        annotate("text", x=151.7817, y=-33.1, label = "Newcastle")+
        geom_point(aes(x=149.5775, y=-33.4193), col="red")+    # Bathurst
        annotate("text", x=149.5775, y=-33.8, label = "Bathurst")+
        geom_point(aes(x=147.3598, y=-35.1082), col="red")+    # Wagga Wagga
        annotate("text", x=147.3598, y=-35.5, label = "Wagga Wagga")+
        geom_point(aes(x=149.7812, y=-30.3324), col="red")+    # Narrabri
        annotate("text", x=149.7812, y=-30.8, label = "Narrabri")+
        geom_point(aes(x=152.9, y=-31.4333), col="red")+    # Port Macquarie
        annotate("text", x=152.9, y=-31.9, label = "Port Macquarie")+
        geom_point(aes(x=145.9378, y=-30.0888), col="red")+    # Bourke
        annotate("text", x=145.9378, y=-30.5, label = "Bourke")+
        geom_point(aes(x=153.4, y=-28.0167), col="red")+    # Gold Coast
        annotate("text", x=153.4, y=-28.5, label = "Gold Coast")+
        geom_point(aes(x=146.0455, y=-34.2801), col="red")+    # Griffith
        annotate("text", x=146.0455, y=-34.6, label = "Griffith")+
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
        guides(color = guide_legend(nrow=5, byrow = T))+
        xlim(user.lon.min, user.lon.max)+
        ylim(user.lat.min, user.lat.max)
    
    
    ### wind severity percentile
    p5 <- ggplot(aus.poly) +
        geom_point(wind.severity, mapping=aes(lon, lat, 
                                              fill=as.character(wind.severity.percentile)), 
                   pch = 21, size = 5)+
        geom_sf(fill=NA) +
        geom_point(aes(x=151.2093, y=-33.8688), col="red")+  # sydney
        annotate("text", x=151.2093, y=-34.2, label = "Sydney")+
        geom_point(aes(x=149.13, y=-35.2809), col="red")+    # canberra
        annotate("text", x=149.13, y=-35.5, label = "Canberra")+
        geom_point(aes(x=151.7817, y=-32.9283), col="red")+    # new castle
        annotate("text", x=151.7817, y=-33.1, label = "Newcastle")+
        geom_point(aes(x=149.5775, y=-33.4193), col="red")+    # Bathurst
        annotate("text", x=149.5775, y=-33.8, label = "Bathurst")+
        geom_point(aes(x=147.3598, y=-35.1082), col="red")+    # Wagga Wagga
        annotate("text", x=147.3598, y=-35.5, label = "Wagga Wagga")+
        geom_point(aes(x=149.7812, y=-30.3324), col="red")+    # Narrabri
        annotate("text", x=149.7812, y=-30.8, label = "Narrabri")+
        geom_point(aes(x=152.9, y=-31.4333), col="red")+    # Port Macquarie
        annotate("text", x=152.9, y=-31.9, label = "Port Macquarie")+
        geom_point(aes(x=145.9378, y=-30.0888), col="red")+    # Bourke
        annotate("text", x=145.9378, y=-30.5, label = "Bourke")+
        geom_point(aes(x=153.4, y=-28.0167), col="red")+    # Gold Coast
        annotate("text", x=153.4, y=-28.5, label = "Gold Coast")+
        geom_point(aes(x=146.0455, y=-34.2801), col="red")+    # Griffith
        annotate("text", x=146.0455, y=-34.6, label = "Griffith")+
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
        xlim(user.lon.min, user.lon.max)+
        ylim(user.lat.min, user.lat.max)
    
    ### wind speed intensity (0.1 knots)
    p6 <- ggplot(aus.poly) +
        geom_point(wind.severity, mapping=aes(lon, lat, fill=selected.wind.speed), 
                   pch = 21, size = 5)+
        geom_sf(fill=NA) +
        geom_point(aes(x=151.2093, y=-33.8688), col="red")+  # sydney
        annotate("text", x=151.2093, y=-34.2, label = "Sydney")+
        geom_point(aes(x=149.13, y=-35.2809), col="red")+    # canberra
        annotate("text", x=149.13, y=-35.5, label = "Canberra")+
        geom_point(aes(x=151.7817, y=-32.9283), col="red")+    # new castle
        annotate("text", x=151.7817, y=-33.1, label = "Newcastle")+
        geom_point(aes(x=149.5775, y=-33.4193), col="red")+    # Bathurst
        annotate("text", x=149.5775, y=-33.8, label = "Bathurst")+
        geom_point(aes(x=147.3598, y=-35.1082), col="red")+    # Wagga Wagga
        annotate("text", x=147.3598, y=-35.5, label = "Wagga Wagga")+
        geom_point(aes(x=149.7812, y=-30.3324), col="red")+    # Narrabri
        annotate("text", x=149.7812, y=-30.8, label = "Narrabri")+
        geom_point(aes(x=152.9, y=-31.4333), col="red")+    # Port Macquarie
        annotate("text", x=152.9, y=-31.9, label = "Port Macquarie")+
        geom_point(aes(x=145.9378, y=-30.0888), col="red")+    # Bourke
        annotate("text", x=145.9378, y=-30.5, label = "Bourke")+
        geom_point(aes(x=153.4, y=-28.0167), col="red")+    # Gold Coast
        annotate("text", x=153.4, y=-28.5, label = "Gold Coast")+
        geom_point(aes(x=146.0455, y=-34.2801), col="red")+    # Griffith
        annotate("text", x=146.0455, y=-34.6, label = "Griffith")+
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
        ggtitle(paste0("Max. wind speed (0.1 knots)"))+
        xlim(user.lon.min, user.lon.max)+
        ylim(user.lat.min, user.lat.max)
    
    ### save plot
    jpeg(paste0(destDir, "/", user.region.name, "_", date.of.interest,
               "_storm_", storm.duration,
               "_drought_", drought.duration, ".jpg"), units="in", res=150,width = 16, height=20)
    
    
    grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 3)
    
    dev.off()
    
}
