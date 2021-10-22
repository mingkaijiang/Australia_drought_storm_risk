extract_selected_sites <- function(sourceDir, 
                                   destDir,
                                   varName,
                                   siteDF,
                                   user.region.name) {
    
    #### Create output folder
    if(!dir.exists(destDir)) {
        dir.create(destDir, showWarnings = FALSE)
    }
    
    ### prepare year DFs
    yr.list <- c(1971:2020)
    n.yr <- length(yr.list)
    
    ### number of leap years
    lp.year <- 13
    n.days <- lp.year + n.yr * 365 - 31
    
    ### prepare all input file path
    dayDF <- data.frame(seq.Date(as.Date("1971/01/01"), 
                                 as.Date("2020/11/30"), 
                                 by="day"),
                        NA, NA, NA)
    colnames(dayDF) <- c("Date", "Year", "Lab", "Path")
    dayDF$Year <- year(dayDF$Date)
    dayDF$Lab <- gsub("-", "", dayDF$Date)
    
    if (varName == "rain") {
        dayDF$Path <- paste0(sourceDir, dayDF$Year, "/rain_", 
                             dayDF$Lab, ".grid")
    } else if (varName == "tmax") {
        dayDF$Path <- paste0(sourceDir, dayDF$Year, "/", dayDF$Lab,
                             dayDF$Lab, ".grid")
    } else if (varName == "vp3pm") {
        dayDF$Path <- paste0(sourceDir, dayDF$Year, "/", dayDF$Lab,
                             dayDF$Lab, ".grid")
    } else {
        print(paste0("no rule for ", varName))
    }
    
    
    ### get subset lat information
    lat.list <- siteDF$latID
    lat.length <- length(lat.list)
    lat.id <- 1:lat.length
    
    lon.list <- siteDF$lonID
    lon.length <- length(lon.list)
    lon.id <- 1:lon.length
    
    ### create out storage matrix
    out <- array(NA, c(lat.length, lon.length, n.days))
    
    ### read in data
    for (i in 1:n.days) {
        
        ## read in data
        inName <- dayDF[i,"Path"]
        myDF <- read.ascii.grid(inName)
        
        for (j in lat.id) {
            for (k in lon.id) {
                j2 <- lat.list[j]
                k2 <- lon.list[k]
                ### save data
                out[j, k, i] <- myDF$data[j2,k2]
            } # k loop
        } # j loop
    } # i loop
    
    ### save output
    saveRDS(out, file=paste0(destDir, "/", varName, "_", user.region.name, "_sites.rds"))
    
}   # function loop


