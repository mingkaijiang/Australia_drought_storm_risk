#### Create output folder
if(!dir.exists("output")) {
    dir.create("output", showWarnings = FALSE)
}

#### Install packages
if(!require(pacman))install.packages("pacman")
pacman::p_load(raster,
               ncdf4,
               spatstat,
               lattice,
               fields,
               matrixStats,
               RColorBrewer,
               ggplot2,
               cowplot,
               doBy,
               chillR,
               RSAGA,
               lubridate)    


#### Sourcing all R files in the modules subdirectory
sourcefiles1 <- dir("functions", pattern="[.]R$", recursive = TRUE, full.names = TRUE)
for(z in sourcefiles1)source(z)


