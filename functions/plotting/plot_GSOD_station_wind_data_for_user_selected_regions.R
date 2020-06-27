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
        geom_point(myDF, mapping=aes(LON, LAT, fill=max.wind), pch = 21)+
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
        scale_fill_viridis_b(name="max wind speed")+
        ggtitle(paste0("Max wind speed (0.1 knots)"))+
        xlim(user.lon.min, user.lon.max)+
        ylim(user.lat.min, user.lat.max)
    
    ### plot
    jpeg(paste0(destDir, "/GSOD_max_wind_speed_", user.region.name, ".jpg"), 
         units="in", res=150,width = 6, height=6)
    plot(p1)    
    dev.off()
    

}