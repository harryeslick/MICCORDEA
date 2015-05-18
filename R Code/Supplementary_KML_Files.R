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
bb.breaks <- seq(-0.79, 0.71, by = 0.3)
ya.breaks <- seq(0, 7)

##### Convert to SpatialPixelsDataFrame for easier KML generation
#change in yield
a2.2030.change <- as(tz.bb.change[[1]], "SpatialPixelsDataFrame")
a2.2050.change <- as(tz.bb.change[[2]], "SpatialPixelsDataFrame")
a1b.2030.change <- as(tz.bb.change[[3]], "SpatialPixelsDataFrame")
a1b.2050.change <- as(tz.bb.change[[4]], "SpatialPixelsDataFrame")
b1.2030.change <- as(tz.bb.change[[5]], "SpatialPixelsDataFrame")
b1.2050.change <- as(tz.bb.change[[6]], "SpatialPixelsDataFrame")

#attainable yields for each time slice
a2.2030.ya <- as(tz.ya[[1]], "SpatialPixelsDataFrame")
a2.2050.ya <- as(tz.ya[[2]], "SpatialPixelsDataFrame")
a1b.2030.ya <- as(tz.ya[[3]], "SpatialPixelsDataFrame")
a1b.2050.ya <- as(tz.ya[[4]], "SpatialPixelsDataFrame")
b1.2030.ya <- as(tz.ya[[5]], "SpatialPixelsDataFrame")
b1.2050.ya <- as(tz.ya[[6]], "SpatialPixelsDataFrame")
base.ya <- as(tz.ya[[7]], "SpatialPixelsDataFrame")

##### Reproject spatial data frame objects for export to GoogleEarth
a2.2030.change <- reproject(a2.2030.change)
a2.2050.change <- reproject(a2.2050.change)
a1b.2030.change <- reproject(a1b.2030.change)
a1b.2050.change <- reproject(a1b.2050.change)
b1.2030.change <- reproject(b1.2030.change)
b1.2050.change <- reproject(b1.2050.change)

a2.2030.ya <- reproject(a2.2030.ya)
a2.2050.ya <- reproject(a2.2050.ya)
a1b.2030.ya <- reproject(a1b.2030.ya)
a1b.2050.ya <- reproject(a1b.2050.ya)
b1.2030.ya <- reproject(b1.2030.ya)
b1.2050.ya <- reproject(b1.2050.ya)
base.ya <- reproject(base.ya)

##### Cut objects for plotting
#cut change in yield losses
a2.2030.change$cuts <- cut(a2.2030.change$layer.1.1, breaks = bb.breaks,
                           include.lowest = TRUE)
a2.2050.change$cuts <- cut(a2.2050.change$layer.2.1, breaks = bb.breaks,
                           include.lowest = TRUE)
a1b.2030.change$cuts <- cut(a1b.2030.change$layer.1.2, breaks = bb.breaks,
                            include.lowest = TRUE)
a1b.2050.change$cuts <- cut(a1b.2050.change$layer.2.2, breaks = bb.breaks,
                            include.lowest = TRUE)
b1.2030.change$cuts <- cut(b1.2030.change$layer.1, breaks = bb.breaks,
                           include.lowest = TRUE)
b1.2050.change$cuts <- cut(b1.2050.change$layer.2, breaks = bb.breaks,
                           include.lowest = TRUE)
#cut attainable yields
a2.2030.ya$cuts <- cut(a2.2030.ya$a230_att, breaks = ya.breaks,
                           include.lowest = TRUE)
a2.2050.ya$cuts <- cut(a2.2050.ya$a250_att, breaks = ya.breaks,
                           include.lowest = TRUE)
a1b.2030.ya$cuts <- cut(a1b.2030.ya$ab30_att, breaks = ya.breaks,
                            include.lowest = TRUE)
a1b.2050.ya$cuts <- cut(a1b.2050.ya$ab50_att, breaks = ya.breaks,
                            include.lowest = TRUE)
b1.2030.ya$cuts <- cut(b1.2030.ya$b130_att, breaks = ya.breaks,
                           include.lowest = TRUE)
b1.2050.ya$cuts <- cut(b1.2050.ya$b150_att, breaks = ya.breaks,
                           include.lowest = TRUE)
base.ya$cuts <- cut(base.ya$base_att, breaks = ya.breaks,
                       include.lowest = TRUE)

#### Begin KML export and visualize in GoogleEarth ####
##set up a few items so that our KML file outputs match Figure 7 in manuscript

#set palette
mypalette.bb <- colorRampPalette(brewer.pal(length(bb.breaks-1), "RdYlBu"), 
                                 space = "Lab")

mypalette.ya <- colorRampPalette(brewer.pal(length(ya.breaks-1), "Oranges"), 
                                 space = "Lab")

#### End setup ####

#set working directory to KML, no apparent easy way to specificy this in file name
setwd("../KML")

#create the yield loss KML file
kml_open("Tanzania_BB_Change_(Figure_7).kml", overwrite = TRUE)
kml_layer(a2.2030.change["cuts"],
          subfolder.name = "A2 2030 Change",
          layer.name = "A2 2030 Change",
          raster_name = "a2_2030.png",
          plot.legend = FALSE,
          colour_scale = mypalette.bb(length(levels(a2.2030.change$cuts))))
kml_layer(a2.2050.change["cuts"],
          subfolder.name = "A2 2530 Change",
          layer.name = "A2 2050 Change",
          raster_name = "a2_2050.png",
          plot.legend = FALSE,
          colour_scale = mypalette.bb(length(levels(a2.2050.change$cuts))))
kml_layer(a1b.2030.change["cuts"],
          subfolder.name = "A1B 2030 Change",
          layer.name = "A1B 2030 Change",
          raster_name = "a1b_2030.png",
          plot.legend = FALSE,
          colour_scale = mypalette.bb(length(levels(a1b.2030.change$cuts))))
kml_layer(a1b.2050.change["cuts"],
          subfolder.name = "A1B 2050 Change",
          layer.name = "A1B 2050 Change",
          raster_name = "a1b_2050.png",
          plot.legend = FALSE,
          colour_scale = mypalette.bb(length(levels(a1b.2050.change$cuts))))
kml_layer(b1.2030.change["cuts"],
          subfolder.name = "B1 2030 Change",
          layer.name = "B1 2030 Change",
          raster_name = "b1_2030.png",
          plot.legend = FALSE,
          colour_scale = mypalette.bb(length(levels(b1.2030.change$cuts))))
kml_layer(b1.2050.change["cuts"],
          subfolder.name = "B1 2050 Change",
          layer.name = "B1 2050 Change",
          raster_name = "b1_2050.png",
          colour_scale = mypalette.bb(length(levels(b1.2050.change$cuts))))
kml_close("Tanzania_BB_Change.kml")

#Attainable yield KML file
kml_open("Tanzania_attainable_yield.kml", overwrite = TRUE)
kml_layer(base.ya["cuts"],
          subfolder.name = "Base Attainable Yield",
          layer.name = "Base Attainable Yield",
          raster_name = "base_attainable_yield.png",
          plot.legend = FALSE,
          colour_scale = mypalette.ya(length(levels(base.ya$cuts))))
kml_layer(a2.2030.ya["cuts"],
          subfolder.name = "A2 2030 Attainable Yield",
          layer.name = "A2 2030 Attainable Yield",
          raster_name = "a2_2030_attainable_yield.png",
          plot.legend = FALSE,
          colour_scale = mypalette.ya(length(levels(a2.2030.ya$cuts))))
kml_layer(a2.2050.ya["cuts"],
          subfolder.name = "A2 2050 Attainable Yield",
          layer.name = "A2 2050 Attainable Yield",
          raster_name = "a2_2050_attainable_yield.png",
          plot.legend = FALSE,
          colour_scale = mypalette.ya(length(levels(a2.2050.ya$cuts))))
kml_layer(a1b.2030.ya["cuts"],
          subfolder.name = "A1B 2030 Attainable Yield",
          layer.name = "A1B 2030 Attainable Yield",
          raster_name = "a1b_2030_attainable_yield.png",
          plot.legend = FALSE,
          colour_scale = mypalette.ya(length(levels(a1b.2030.ya$cuts))))
kml_layer(a1b.2050.ya["cuts"],
          subfolder.name = "A1B 2050 Attainable Yield",
          layer.name = "A1B 2050 Attainable Yield",
          raster_name = "a1b_2050_attainable_yield.png",
          plot.legend = FALSE,
          colour_scale = mypalette.ya(length(levels(a1b.2050.ya$cuts))))
kml_layer(b1.2030.ya["cuts"],
          subfolder.name = "B1 2030 Attainable Yield",
          layer.name = "B1 2030 Attainable Yield",
          raster_name = "b1_2030_attainable_yield.png",
          plot.legend = FALSE,
          colour_scale = mypalette.ya(length(levels(b1.2030.ya$cuts))))
kml_layer(b1.2050.ya["cuts"],
          subfolder.name = "B1 2050 Attainable Yield",
          layer.name = "B1 2050 Attainable Yield",
          raster_name = "b1_2050_attainable_yield.png",
          colour_scale = mypalette.ya(length(levels(b1.2050.ya$cuts))))
kml_close("Tanzania_attainable_yield.kml")

#reset working directory for further use with R code
setwd("../R Code")

#### End KML export and visualize in GoogleEarth ####

#### The resulting KML files are now found in the KML folder and can be     ####
#### viewed using GoogleEarth                                               ####

#eos
