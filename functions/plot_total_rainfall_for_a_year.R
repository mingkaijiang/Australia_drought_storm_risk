plot_total_rainfall_for_a_year <- function(sourceDir, destDir,
                                           user.defined.year) {
    
    ### grid information
    lat.id <- c(1:691)
    lat.lab <- paste0("lat", lat.id)
    
    lon.id <- c(1:886)
    lon.lab <- paste0("lon", lon.id)
    
    lon <- seq(111.975, 111.975 + (0.05 * 885), by=0.05)
    #lat <- seq(-44.525, -44.525 + (0.05 * 690), by=0.05)
    lat <- seq(-10.025, -10.025 + (-0.05 * 690), by=-0.05)
    
    ### create lon lat DF for future plotting
    latlonDF <- data.frame(rep(lat.id, each = max(lon.id)),
                           rep(lon.id, max(lat.id)), 
                           rep(lat, each = max(lon.id)),
                           rep(lon, max(lat.id)))
    colnames(latlonDF) <- c("latID", "lonID", "lat", "lon")
    
    ### prepare all input file path
    dayDF <- data.frame(seq.Date(as.Date("1900/01/01"), 
                                 as.Date("2020/03/31"), 
                                 by="day"),
                        NA, NA, NA)
    colnames(dayDF) <- c("Date", "Year", "Lab", "Path")
    dayDF$Year <- year(dayDF$Date)
    dayDF$Lab <- gsub("-", "", dayDF$Date)
    dayDF$Path <- paste0(sourceDir, dayDF$Year, "/rain_", 
                         dayDF$Lab, ".grid")
    
    ### subset according to year
    subDF <- subset(dayDF, Year == user.defined.year)
    n.days <- dim(subDF)[1]
    
    ### prepare storage DF
    ### create out storage matrix
    out <- array(NA, c(length(lat.id), length(lon.id)))
    
    ### calculate annual sum rainfall
    for (i in c(1:n.days)) {
        
        ## read in data
        inName <- subDF[i,"Path"]
        myDF <- read.ascii.grid(inName)
        
        ### matrix addition
        out <- matrix(mapply(sum, out, myDF$data, MoreArgs=list(na.rm=T)), ncol=length(lon.id))
    }
    
    ### save output
    saveRDS(out, file=paste0(destDir, "/Australia_total_rainfall_year_",
                             user.defined.year, ".rds"))
    
    ### convert into long format
    outDF <- melt(out)
    colnames(outDF) <- c("latID", "lonID", "value")
    outDF <- merge(outDF, latlonDF,
                   by=c("latID", "lonID"))
    
    outDF <- outDF[order(outDF$lonID, outDF$latID),]

    ### prepare ssf grids
    subDF <- subset(outDF, value == 0)
    subDF <- subDF[order(subDF$lon, subDF$lat),]
    
    
    ### read in sea surface mask
    ssf.raster <- read_sea_surface_mask()
    DF1 <- latlonDF[,c("lon", "lat")]
    ssfDF <- cbind(DF1, extract(ssf.raster, DF1, df=T))
    
    ### merge ssf and input DF and then remove sea surface
    outDF.test <- merge(outDF, ssfDF, by=c("lon", "lat"), all=T)
    outDF.test <- subset(outDF.test, ssf == 1)
    outDF.test <- outDF.test[order(outDF.test$lon, outDF.test$lat),]
    
    ### prepare discrete plotting scheme
    ### get the value breaks
    value_brks <- round(quantile(outDF$value, 
                                 probs = seq(0, 1, 100/11/100)), 2)
    
    value_brks <- unique(value_brks)
    
    #### create categorical plotting labels for each plotting variables
    outDF.test$value_cat <- cut(outDF.test$value, 
                          breaks = value_brks)
    
    ### remove NAs
    outDF.test <- outDF.test[!is.na(outDF.test$value_cat),]
    
    ### brk labels
    value_lab <- as.character(rev(unique(outDF.test$value_cat)))
    value_lab <- gsub(",", " to ", value_lab)
    value_lab <- gsub("]", "", value_lab)
    value_lab <- sub('.', '', value_lab)
    
    ### ordering
    test1 <- gsub( " .*$", "", value_lab)
    test2 <- sub(".+? ", "", value_lab)
    test3 <- gsub("to ", "", test2)
    tmp <- data.frame(cbind(test1, value_lab, test3))
    tmp$test1 <- as.numeric(as.character(tmp$test1))
    tmp$test3 <- as.numeric(as.character(tmp$test3))
    
    tmp <- tmp[order(tmp$test1),]
    tmp$value_lab <- paste0(tmp$test1, " to ", tmp$test3)
    
    value_lab <- tmp$value_lab
    n.discrete.colors <- length(value_lab)
    rain.color <- rev(brewer.pal(n = n.discrete.colors, name = "Blues"))
    
    ### plot 1-year rainfall total
    p1 <- ggplot(outDF.test, aes(lon, lat)) +
        geom_tile(aes(fill=value_cat))+
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
              legend.position="right",
              legend.box = 'vertical',
              legend.box.just = 'left')+
        scale_fill_manual(name="value",
                          values=rev(rain.color),
                          labels=value_lab)+
        guides(color = guide_legend(nrow=5, byrow = T))+
        ggtitle(paste0("Year ", user.defined.year, " total rainfall"))
    
    ### save image
    jpeg(paste0(destDir, "/Australia_Year_", user.defined.year,
                "_rainfall.jpg"), units="in", res=150,width = 6, height=6)
    plot(p1)
    dev.off()
    
    ### it seems that ssf layer is problematic.
    ### next to figure out how to extract seas from the dataframe
    ### also, the reverse sign of latitude to make the plot - need to update throughout all figures

}