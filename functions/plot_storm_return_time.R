plot_storm_return_time <- function(sourceDir, destDir,  user.region.name,
                                                        date.of.interest,
                                                        user.lat.max,
                                                        user.lat.min,
                                                        user.lon.max,
                                                        user.lon.min,
                                                        storm.duration) {

        
    #### Create output folder
    if(!dir.exists(destDir)) {
        dir.create(destDir, showWarnings = FALSE)
    }
    
    ### translate date of interest into nday information
    date.list <- seq.Date(as.Date("1900/01/01"), 
                          as.Date("2020/03/31"), 
                          by="day")
    date.list <- gsub("-", "", date.list)
    nday <- match(date.of.interest, date.list)
    
    ### print statement
    if (is.na(nday)) {
        stop(paste0("no ", date.of.interest, " in the dataset"))
    } else {
        print(paste0("Found ", date.of.interest, 
                     " in the dataset, proceed to check storm event severity"))
    }
    
    ### read in the R database
    myData <- readRDS(paste0(sourceDir, "/", user.region.name, "_regions.rds"))
    stormData <- readRDS(paste0(destDir, "/Storm_extreme_return_time_", storm.duration, "_",
                                user.region.name, "_regions.rds"))
    
    ### dimension information
    dim1 <- dim(myData)[1]
    dim2 <- dim(myData)[2]
    dim3 <- dim(myData)[3]
    
    ### prepare storage DF to store extreme drought and storm information
    storm.on.date.of.interest <- array(NA, c(dim1, dim2))
    storm.severity.on.date.of.interest <- array(NA, c(dim1, dim2))
    
    ### loop each grid
    for (i in c(1:dim1)) {
        for (j in c(1:dim2)) {
            
            ### calculate storm on date of interest, based on pre-defined duration threshold
            ### get storm intensity based on duration threshold
            ### get the rainfall data for each grid
            storm.on.date.of.interest[i,j] <- sum(c(myData[i,j,nday], 
                                                  myData[i,j,nday+storm.duration-1]), na.rm=T)
            
            ### get storm extreme index information
            storm.P99 <- ifelse(is.na(stormData[i,j,1]), 0, stormData[i,j,1])
            storm.P98 <- ifelse(is.na(stormData[i,j,2]), 0, stormData[i,j,2])
            storm.P95 <- ifelse(is.na(stormData[i,j,3]), 0, stormData[i,j,3])
            storm.P90 <- ifelse(is.na(stormData[i,j,4]), 0, stormData[i,j,4])
            storm.P80 <- ifelse(is.na(stormData[i,j,5]), 0, stormData[i,j,5])
            storm.P70 <- ifelse(is.na(stormData[i,j,6]), 0, stormData[i,j,6])
            storm.P60 <- ifelse(is.na(stormData[i,j,7]), 0, stormData[i,j,7])
            storm.P50 <- ifelse(is.na(stormData[i,j,8]), 0, stormData[i,j,8])
            storm.P40 <- ifelse(is.na(stormData[i,j,9]), 0, stormData[i,j,9])
                
            ### check storm severity
            if (storm.on.date.of.interest[i,j]>=storm.P99) {
                
                storm.severity.on.date.of.interest[i,j] <- 99.0
                
            } else if (storm.on.date.of.interest[i,j]>=storm.P98 & storm.on.date.of.interest[i,j]<storm.P99) {
                
                storm.severity.on.date.of.interest[i,j] <- 98.0
                
            } else if (storm.on.date.of.interest[i,j]>=storm.P95 & storm.on.date.of.interest[i,j]<storm.P98) {
                
                storm.severity.on.date.of.interest[i,j] <- 95.0
                
            } else if (storm.on.date.of.interest[i,j]>=storm.P90 & storm.on.date.of.interest[i,j]<storm.P95) {
                
                storm.severity.on.date.of.interest[i,j] <- 90.0
                
            } else if (storm.on.date.of.interest[i,j]>=storm.P80 & storm.on.date.of.interest[i,j]<storm.P90) {
                
                storm.severity.on.date.of.interest[i,j] <- 80.0
                
            } else if (storm.on.date.of.interest[i,j]>=storm.P70 & storm.on.date.of.interest[i,j]<storm.P80) {
                
                storm.severity.on.date.of.interest[i,j] <- 70.0
                
            } else if (storm.on.date.of.interest[i,j]>=storm.P60 & storm.on.date.of.interest[i,j]<storm.P70) {
                
                storm.severity.on.date.of.interest[i,j] <- 60.0
                
            } else if (storm.on.date.of.interest[i,j]>=storm.P50 & storm.on.date.of.interest[i,j]<storm.P60) {
                
                storm.severity.on.date.of.interest[i,j] <- 50.0
                
            } else {
                storm.severity.on.date.of.interest[i,j] <- 40.0
                
            }
            
        } # j loop
    } # i loop
    
    ### save output
    saveRDS(storm.severity.on.date.of.interest, 
            file=paste0(destDir, "/storm_severity_", date.of.interest, "_",
                        storm.duration, "_",
                        user.region.name, "_regions.rds"))

    saveRDS(storm.on.date.of.interest, 
            file=paste0(destDir, "/storm_rainfall_", date.of.interest, "_",
                        storm.duration, "_",
                        user.region.name, "_regions.rds"))    

    
    #### read in data
    #storm.severity <- readRDS(paste0(sourceDir, 
    #                                  "/storm_severity_", 
    #                                  date.of.interest, "_",
    #                                  storm.duration, "_",
    #                                 user.region.name, "_regions.rds"))
    storm.severity <- storm.severity.on.date.of.interest
    storm.intensity <- storm.on.date.of.interest  
    
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
    storm.severity.long <- melt(storm.severity)
    colnames(storm.severity.long) <- c("latID", "lonID", "value")
    storm.severity.long <- merge(storm.severity.long, latlonDF.sub,
                                   by=c("latID", "lonID"))
 
    storm.intensity.long <- melt(storm.intensity)
    colnames(storm.intensity.long) <- c("latID", "lonID", "value")
    storm.intensity.long <- merge(storm.intensity.long, latlonDF.sub,
                                 by=c("latID", "lonID"))   
    
    storm.P95.long <- melt(stormData[,,1])
    colnames(storm.P95.long) <- c("latID", "lonID", "value")
    storm.P95.long <- merge(storm.P95.long, latlonDF.sub,
                                 by=c("latID", "lonID"))
    
    ### read in Australia
    aus <- read_Australia_polygon()
    DF1 <- latlonDF[,c("lon", "lat")]
    ausDF <- cbind(DF1, extract(aus, DF1, df=T))
    
    ### merge australia raster and input DF and then remove sea surface
    storm.severity.long <- merge(storm.severity.long, ausDF, by=c("lon", "lat"), all=T)
    storm.severity.long <- subset(storm.severity.long, layer == 1)
    storm.severity.long <- subset(storm.severity.long, value != "NA")

    storm.intensity.long <- merge(storm.intensity.long, ausDF, by=c("lon", "lat"), all=T)
    storm.intensity.long <- subset(storm.intensity.long, layer == 1)
    storm.intensity.long <- subset(storm.intensity.long, value != "NA")
    
    storm.P95.long <- merge(storm.P95.long, ausDF, by=c("lon", "lat"), all=T)
    storm.P95.long <- subset(storm.P95.long, layer == 1)
    storm.P95.long <- subset(storm.P95.long, value != "NA")   
    
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
                          limits=c("99", "98", "95", "90", "80", "70", "60", "50", "40"),
                          values=rain.color,
                          labels=c("99", "98", "95", "90", "80", "70", "60", "50", "40"))+
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
    
    ### plot storm percentiles
    p3 <- ggplot(aus.poly) +
        geom_tile(storm.P95.long, mapping=aes(lon, lat, fill=as.character(value)))+
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
