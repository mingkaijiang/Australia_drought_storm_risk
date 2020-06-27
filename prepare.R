
#### Create input folder
if(!dir.exists("input")) {
    dir.create("input", showWarnings = FALSE)
}


#### Create output folder
if(!dir.exists("output")) {
    dir.create("output", showWarnings = FALSE)
}

### Create plots folder
if(!dir.exists("plots")) {
    dir.create("plots", showWarnings = FALSE)
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
               lubridate,
               reshape2,
               grid,
               gridExtra,
               rgeos,
               rnaturalearthdata,
               rnaturalearth,
               AWAPer,
               SPEI)    


#### Sourcing all R files in the modules subdirectory
sourcefiles1 <- dir("functions", pattern="[.]R$", recursive = TRUE, full.names = TRUE)
for(z in sourcefiles1)source(z)


