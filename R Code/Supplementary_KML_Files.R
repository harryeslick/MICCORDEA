##############################################################################
# title         : Supplementary_KML_Files.R;
# purpose       : generate supplementary KML files for publication on web;
# producer      : prepared by A. Sparks;
# last update   : IRRI, Los Baños, May 2015;
# inputs        : ESRI files of yield losses and attainable yield for Tanzania 
#               : calculated using RICEPEST;
# outputs       : KML files of yield losses for base/2030/2050 a2/b1/ab scenario
#                 and attainable yields for each time-slice in absence of disease;
# remarks 1     : ;
# Licence:      : GPL2;
##############################################################################

#### Begin libraries ####
library(raster)
library(plotKML)
library(RColorBrewer)

#### End libraries ####

#### Begin data import ####

#load Raster files and set CRS
tz.bb <- stack(list.files(path = "../Data/RICEPEST Modified GPS3 Output", 
                          pattern = "^[a,b].*bb$", full.names = TRUE))
crs(tz.bb) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
tz.lb <- stack(list.files(path = "../Data/RICEPEST Modified GPS3 Output", 
                          pattern = "^[a,b].*lb$", full.names = TRUE))
crs(tz.lb) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
tz.ya <- stack(list.files(path = "../Data/RICEPEST Modified GPS3 Output", 
                          pattern = "^[a,b].*att$", full.names = TRUE))
crs(tz.ya) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

#### End data import ####

#### Begin data manipulation ####

#caclulate loss due to BB
tz.bb.loss <- (tz.ya-tz.bb)

#calculate change
for(i in 1:6){
  change <- tz.bb.loss[[i]]-tz.bb.loss[[7]]
  if(i == 1){tz.bb.change <- change} else 
    tz.bb.change <- stack(tz.bb.change, change)
}

#convert values to classes and cut for plotting
bb.breaks <- data.frame(seq(1:6), seq(-0.79, 0.56, by = 0.25))

list1 <- unstack(tz.bb.change)

#convert to SpatialPixelsDataFrame for easier KML generation
a2.2030 <- as(list1[[1]], "SpatialPixelsDataFrame")
a2.2050 <- as(list1[[2]], "SpatialPixelsDataFrame")
a1b.2030 <- as(list1[[3]], "SpatialPixelsDataFrame")
a1b.2050 <- as(list1[[4]], "SpatialPixelsDataFrame")
b1.2030 <- as(list1[[5]], "SpatialPixelsDataFrame")
b1.2050 <- as(list1[[6]], "SpatialPixelsDataFrame")

#cut objects for plotting
a2.2030$cuts <- cut(a2.2030$layer.1.1, breaks = bb.breaks[, 2],
                    include.lowest = TRUE)
a2.2050$cuts <- cut(a2.2050$layer.2.1, breaks = bb.breaks[, 2],
                    include.lowest = TRUE)
a1b.2030$cuts <- cut(a1b.2030$layer.1.2, breaks = bb.breaks[, 2],
                    include.lowest = TRUE)
a1b.2050$cuts <- cut(a1b.2050$layer.2.2, breaks = bb.breaks[, 2],
                    include.lowest = TRUE)
b1.2030$cuts <- cut(b1.2030$layer.1, breaks = bb.breaks[, 2],
                    include.lowest = TRUE)
b1.2050$cuts <- cut(b1.2050$layer.2, breaks = bb.breaks[, 2],
                    include.lowest = TRUE)
#### Begin KML export and visualize in GoogleEarth ####
##set up a few items so that our KML file outputs match Figure 7 in manuscript

#set palette
NColorBreaks <- length(bb.breaks[, 1])-1
mypalette <- colorRampPalette(brewer.pal(NColorBreaks, "RdYlBu"), space = "Lab")

#### End setup ####
#change working directory for saving KML files
setwd("../KML")

#create the KML files
plotKML(a2.2030["cuts"], 
        folder.name = "../KML",
        file.name = "a2_2030_bb_change.kml",
        colour_scale = mypalette(length(levels(a2.2030$cuts))))

#### End KML export and visualize in GoogleEarth ####

#eos