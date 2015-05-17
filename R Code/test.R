##############################################################################
# title         : Supplementary_KML_Files.R;
# purpose       : generate supplementary KML files for publication on web;
# producer      : prepared by A. Sparks;
# last update   : IRRI, Los Baños, May 2015;
# inputs        : ESRI files of yield losses and attainable yield for Tanzania calculated using RICEPEST;
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
crs(tz.bb) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
tz.lb <- stack(list.files(path = "../Data/RICEPEST Modified GPS3 Output", 
                          pattern = "^[a,b].*lb$", full.names = TRUE))
crs(tz.lb) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
tz.ya <- stack(list.files(path = "../Data/RICEPEST Modified GPS3 Output", 
                          pattern = "^[a,b].*att$", full.names = TRUE))
crs(tz.ya) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

#### End data import ####

#### Begin data manipulation ####

#caclulate loss due to BB
tz.bb.loss <- (tz.ya-tz.bb)

#calculate change
for(i in 1:6){
  change <- tz.bb.loss[[i]]-tz.bb.loss[[7]]
  if(i == 1){tz.bb.change <- change} else tz.bb.change <- stack(tz.bb.change, change)
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

#reproject spatial data frame objects for export to GoogleEarth
a2.2030 <- reproject(a2.2030)
a2.2050 <- reproject(a2.2050)
a1b.2030 <- reproject(a1b.2030)
a1b.2050 <- reproject(a1b.2050)
b1.2030 <- reproject(b1.2030)
b1.2050 <- reproject(b1.2050)

#cut objects for plotting
a2.2030$cuts <- cut(a2.2030$layer.1.1, breaks = bb.breaks[, 2],
                    include.lowest = TRUE)
a2.2050$cuts <- cut(a2.2050$layer.2.1, breaks = bb.breaks[, 2],
                    include.lowest = TRUE)
a1b.2050$cuts <- cut(a1b.2030$layer.1.2, breaks = bb.breaks[, 2],
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
#create the KML files

setwd("../KML")

kml_open("Tanzania_BB_Change.kml")
kml_layer(a2.2030["cuts"],
          subfolder.name = "A2 2030 Change",
          layer.name = "A2 2030 Change",
          raster_name = "a2_2030.png",
          colour_scale = mypalette(length(levels(a2.2030$cuts))))
kml_layer(a2.2050["cuts"],
          subfolder.name = "A2 2530 Change",
          layer.name = "A2 2050 Change",
          raster_name = "a2_2050.png",
          plot.legend = FALSE,
          colour_scale = mypalette(length(levels(a2.2050$cuts))))
kml_layer(a1b.2030["cuts"],
          subfolder.name = "A1B 2030 Change",
          layer.name = "A1B 2030 Change",
          raster_name = "a1b_2030.png",
          plot.legend = FALSE,
        colour_scale = mypalette(length(levels(a1b.2030$cuts))))
kml_layer(a1b.2050["cuts"],
          subfolder.name = "A1B 2050 Change",
          layer.name = "A1B 2050 Change",
          raster_name = "a1b_2050.png",
          plot.legend = FALSE,
        colour_scale = mypalette(length(levels(a1b.2050$cuts))))
kml_layer(b1.2030["cuts"],
          subfolder.name = "B1 2030 Change",
          layer.name = "B1 2030 Change",
          raster_name = "b1_2030.png",
          plot.legend = FALSE,
        colour_scale = mypalette(length(levels(b1.2030$cuts))))
kml_layer(b1.2050["cuts"],
          subfolder.name = "B1 2050 Change",
          layer.name = "B1 2050 Change",
          raster_name = "b1_2050.png",
          plot.legend = FALSE,
        colour_scale = mypalette(length(levels(b1.2050$cuts))))
kml_close("Tanzania_BB_Change.kml")

setwd("../R Code")
#### End KML export and visualize in GoogleEarth ####

#### The resulting KML file is now found in the KML folder ####

#eos
