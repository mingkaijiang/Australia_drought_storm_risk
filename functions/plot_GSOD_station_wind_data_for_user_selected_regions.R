plot_GSOD_station_wind_data_for_user_selected_regions <- function(sourceDir,
                                                                  destDir,
                                                                  user.region.name,
                                                                  user.lat.max,
                                                                  user.lat.min,
                                                                  user.lon.max,
                                                                  user.lon.min) {
    
    #### Create output folder
    if(!dir.exists(destDir)) {
        dir.create(destDir, showWarnings = FALSE)
    }
    
    
    ### read in data
    myDF <- read.csv(paste0(sourceDir, "/GSOD_Wind_Extreme_", user.region.name,
                            "_regions.csv"))
    
    ### 
    
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
        scale_fill_manual(name="value",
                          limits=c("99.9", "99", "95", "90", "80", "70", "60", "50", "40"),
                          values=rain.color,
                          labels=c("99.9", "99", "95", "90", "80", "70", "60", "50", "40"))+
        ggtitle(paste0("Storm ", storm.duration, " severity percentile"))+
        guides(color = guide_legend(nrow=5, byrow = T))+
        xlim(user.lon.min, user.lon.max)+
        ylim(user.lat.min, user.lat.max)
    
}