select_station_from_GSOD_global_dataset <- function(destDir,
                                                    user.region.name,
                                                    user.lat.max,
                                                    user.lat.min,
                                                    user.lon.max,
                                                    user.lon.min,
                                                    plot.option) {
    ### Data downloaded from ftp.ncdc.noaa.gov
    
    ### read in station description file 
    stDF <- read.csv("input/isd-history.csv")
    
    ### select stations for Australia based on lon and lat
    ausDF <- subset(stDF, CTRY == "AS")
    
    ausDF <- subset(ausDF, LON <= 155 & LON >= 110 & LAT >= -50)
    
    ### australia polygon
    aus.poly <- ne_countries(scale = "medium", country = "Australia", returnclass = "sf")
    
    
    #### ploting data coverage
    if(plot.option == T) {
        p1 <- ggplot(aus.poly) +
            geom_point(ausDF, mapping=aes(LON, LAT))+
            geom_sf(fill=NA) +
            theme_linedraw() +
            geom_segment(aes(x=user.lon.max, y=user.lat.max, 
                             xend=user.lon.min, yend=user.lat.max), color="red")+
            geom_segment(aes(x=user.lon.min, y=user.lat.max, 
                             xend=user.lon.min, yend=user.lat.min), color="red")+
            geom_segment(aes(x=user.lon.max, y=user.lat.min, 
                             xend=user.lon.min, yend=user.lat.min), color="red")+
            geom_segment(aes(x=user.lon.max, y=user.lat.min, 
                             xend=user.lon.max, yend=user.lat.max), color="red")+
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
            ggtitle(paste0("GSOD Australia stations"))+
            guides(color = guide_legend(nrow=5, byrow = T))+
            xlim(110, 160)+
            ylim(-45, -10)
        
        ### plot
        jpeg(paste0("plots/Australia/GSOD_Australia_stations.jpg"), 
             units="in", res=150,width = 6, height=6)
        plot(p1)    
        dev.off()
    }
    
    
    ### subset stations according to user defined region
    subDF <- subset(ausDF, LON <= user.lon.max & LON >= user.lon.min &
                        LAT >= user.lat.min & LAT <= user.lat.max)
    
    
    ### year information
    subDF$s.year <- substr(subDF$BEGIN, start=1, stop=4)
    subDF$e.year <- substr(subDF$END, start=1, stop=4)
    
    
    ### filter out data with too little data year
    outDF <- subset(subDF, e.year == "2020")
    
    ### add column for wind speed and gust speed
    outDF$max.wind <- NA
    outDF$wind999 <- NA
    outDF$wind99 <- NA
    outDF$wind95 <- NA
    outDF$wind90 <- NA
    outDF$wind80 <- NA
    outDF$wind70 <- NA
    outDF$wind60 <- NA
    outDF$wind50 <- NA
    outDF$wind40 <- NA
    
    outDF$return50 <- NA
    outDF$return40 <- NA
    outDF$return30 <- NA
    outDF$return20 <- NA
    outDF$return10 <- NA
    outDF$return05 <- NA
    outDF$return01 <- NA
    
    ### remove un-useful columns
    outDF$STATE <- NULL
    outDF$ICAO <- NULL
    outDF$CTRY <- NULL
    
    ### data format
    outDF$s.year <- as.numeric(as.character(outDF$s.year))
    outDF$e.year <- as.numeric(as.character(outDF$e.year))
    
    outDF$USAF <- as.numeric(as.character(outDF$USAF))
    outDF$WBAN <- as.numeric(as.character(outDF$WBAN))
    
    ### return
    return(outDF)
}