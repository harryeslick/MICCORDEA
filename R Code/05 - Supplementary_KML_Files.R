################################################################################
# title         : Supplementary_KML_Files.R;
# purpose       : generate supplementary KML files for publication on web;
# producer      : prepared by A. Sparks;
# last update   : IRRI, Los Baños, Jan 2016;
# inputs        : ESRI files of yield losses and attainable yield for Tanzania calculated using RICEPEST;
# outputs       : KML files of yield losses for base/2030/2050 a2/b1/ab scenario
#                 and attainable yields for each time-slice in absence of disease;
# remarks 1     : ;
# Licence:      : GPL2;
################################################################################

# Libraries --------------------------------------------------------------------
library(raster)
library(plotKML)
library(RColorBrewer)

source("Functions/Get_Data.R")
# Load data --------------------------------------------------------------------

download_data()

#load Raster files and set CRS
tz_bb <- stack(list.files(path = "../Data/RICEPEST Modified GPS3 Output",
                          pattern = "^[a,b].*bb$", full.names = TRUE))
crs(tz_bb) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
tz_lb <- stack(list.files(path = "../Data/RICEPEST Modified GPS3 Output",
                          pattern = "^[a,b].*lb$", full.names = TRUE))
crs(tz_lb) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
tz_ya <- stack(list.files(path = "../Data/RICEPEST Modified GPS3 Output",
                          pattern = "^[a,b].*att$", full.names = TRUE))
crs(tz_ya) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"


# Data munging ---- ------------------------------------------------------------

# caclulate loss due to BB
tz_bb_loss <- (tz_ya-tz_bb)

# calculate change in yield losses to blight
for(i in 1:6){
  change <- tz_bb_loss[[i]]-tz_bb_loss[[7]]
  if(i == 1){tz_bb_change <- change} else tz_bb_change <- stack(tz_bb_change, change)
}

# convert values to classes and cut for plotting
bb_breaks <- seq(-0.79, 0.71, by = 0.3)
ya_breaks <- seq(0, 7)

# Convert to SpatialPixelsDataFrame for easier KML generation
# change in yield
a2_2030.change <- as(tz_bb_change[[1]], "SpatialPixelsDataFrame")
a2_2050.change <- as(tz_bb_change[[2]], "SpatialPixelsDataFrame")
a1b_2030.change <- as(tz_bb_change[[3]], "SpatialPixelsDataFrame")
a1b_2050.change <- as(tz_bb_change[[4]], "SpatialPixelsDataFrame")
b1_2030.change <- as(tz_bb_change[[5]], "SpatialPixelsDataFrame")
b1_2050.change <- as(tz_bb_change[[6]], "SpatialPixelsDataFrame")

# attainable yields for each time slice
a2_2030_ya <- as(tz_ya[[1]], "SpatialPixelsDataFrame")
a2_2050_ya <- as(tz_ya[[2]], "SpatialPixelsDataFrame")
a1b_2030_ya <- as(tz_ya[[3]], "SpatialPixelsDataFrame")
a1b_2050_ya <- as(tz_ya[[4]], "SpatialPixelsDataFrame")
b1_2030_ya <- as(tz_ya[[5]], "SpatialPixelsDataFrame")
b1_2050_ya <- as(tz_ya[[6]], "SpatialPixelsDataFrame")
base_ya <- as(tz_ya[[7]], "SpatialPixelsDataFrame")

# Reproject spatial data frame objects for export to GoogleEarth
a2_2030.change <- reproject(a2_2030.change)
a2_2050.change <- reproject(a2_2050.change)
a1b_2030.change <- reproject(a1b_2030.change)
a1b_2050.change <- reproject(a1b_2050.change)
b1_2030.change <- reproject(b1_2030.change)
b1_2050.change <- reproject(b1_2050.change)

a2_2030_ya <- reproject(a2_2030_ya)
a2_2050_ya <- reproject(a2_2050_ya)
a1b_2030_ya <- reproject(a1b_2030_ya)
a1b_2050_ya <- reproject(a1b_2050_ya)
b1_2030_ya <- reproject(b1_2030_ya)
b1_2050_ya <- reproject(b1_2050_ya)
base_ya <- reproject(base_ya)

# Cut objects for plotting
# change in yield losses
a2_2030.change$cuts <- cut(a2_2030.change$layer.1.1, breaks = bb_breaks,
                           include.lowest = TRUE)
a2_2050.change$cuts <- cut(a2_2050.change$layer.2.1, breaks = bb_breaks,
                           include.lowest = TRUE)
a1b_2030.change$cuts <- cut(a1b_2030.change$layer.1.2, breaks = bb_breaks,
                            include.lowest = TRUE)
a1b_2050.change$cuts <- cut(a1b_2050.change$layer.2.2, breaks = bb_breaks,
                            include.lowest = TRUE)
b1_2030.change$cuts <- cut(b1_2030.change$layer.1, breaks = bb_breaks,
                           include.lowest = TRUE)
b1_2050.change$cuts <- cut(b1_2050.change$layer.2, breaks = bb_breaks,
                           include.lowest = TRUE)
# attainable yields
a2_2030_ya$cuts <- cut(a2_2030_ya$a230_att, breaks = ya_breaks,
                           include.lowest = TRUE)
a2_2050_ya$cuts <- cut(a2_2050_ya$a250_att, breaks = ya_breaks,
                           include.lowest = TRUE)
a1b_2030_ya$cuts <- cut(a1b_2030_ya$ab30_att, breaks = ya_breaks,
                            include.lowest = TRUE)
a1b_2050_ya$cuts <- cut(a1b_2050_ya$ab50_att, breaks = ya_breaks,
                            include.lowest = TRUE)
b1_2030_ya$cuts <- cut(b1_2030_ya$b130_att, breaks = ya_breaks,
                           include.lowest = TRUE)
b1_2050_ya$cuts <- cut(b1_2050_ya$b150_att, breaks = ya_breaks,
                           include.lowest = TRUE)
base_ya$cuts <- cut(base_ya$base_att, breaks = ya_breaks,
                       include.lowest = TRUE)

# KML export -------------------------------------------------------------------
# set up a few items so that our KML file outputs match Figure 7 in manuscript

mypalette_bb <- colorRampPalette(brewer.pal(length(bb_breaks-1), "RdYlBu"),
                                 space = "Lab")

mypalette_ya <- colorRampPalette(brewer.pal(length(ya_breaks-1), "Oranges"),
                                 space = "Lab")


# set working directory to KML, no apparent easy way to specificy this in file name
setwd("../KML")

# create the yield loss KML file
kml_open("Tanzania_BB_Change_(Figure_7).kml", overwrite = TRUE)
kml_layer(a2_2030.change["cuts"],
          subfolder.name = "A2 2030 Change",
          layer.name = "A2 2030 Change",
          raster_name = "a2_2030.png",
          plot.legend = FALSE,
          colour_scale = mypalette_bb(length(levels(a2_2030.change$cuts))))
kml_layer(a2_2050.change["cuts"],
          subfolder.name = "A2 2530 Change",
          layer.name = "A2 2050 Change",
          raster_name = "a2_2050.png",
          plot.legend = FALSE,
          colour_scale = mypalette_bb(length(levels(a2_2050.change$cuts))))
kml_layer(a1b_2030.change["cuts"],
          subfolder.name = "A1B 2030 Change",
          layer.name = "A1B 2030 Change",
          raster_name = "a1b_2030.png",
          plot.legend = FALSE,
          colour_scale = mypalette_bb(length(levels(a1b_2030.change$cuts))))
kml_layer(a1b_2050.change["cuts"],
          subfolder.name = "A1B 2050 Change",
          layer.name = "A1B 2050 Change",
          raster_name = "a1b_2050.png",
          plot.legend = FALSE,
          colour_scale = mypalette_bb(length(levels(a1b_2050.change$cuts))))
kml_layer(b1_2030.change["cuts"],
          subfolder.name = "B1 2030 Change",
          layer.name = "B1 2030 Change",
          raster_name = "b1_2030.png",
          plot.legend = FALSE,
          colour_scale = mypalette_bb(length(levels(b1_2030.change$cuts))))
kml_layer(b1_2050.change["cuts"],
          subfolder.name = "B1 2050 Change",
          layer.name = "B1 2050 Change",
          raster_name = "b1_2050.png",
          colour_scale = mypalette_bb(length(levels(b1_2050.change$cuts))))
kml_close("Tanzania_BB_Change.kml")

#Attainable yield KML file
kml_open("Tanzania_attainable_yield.kml", overwrite = TRUE)
kml_layer(base_ya["cuts"],
          subfolder.name = "Base Attainable Yield",
          layer.name = "Base Attainable Yield",
          raster_name = "base_attainable_yield.png",
          plot.legend = FALSE,
          colour_scale = mypalette_ya(length(levels(base_ya$cuts))))
kml_layer(a2_2030_ya["cuts"],
          subfolder.name = "A2 2030 Attainable Yield",
          layer.name = "A2 2030 Attainable Yield",
          raster_name = "a2_2030_attainable_yield.png",
          plot.legend = FALSE,
          colour_scale = mypalette_ya(length(levels(a2_2030_ya$cuts))))
kml_layer(a2_2050_ya["cuts"],
          subfolder.name = "A2 2050 Attainable Yield",
          layer.name = "A2 2050 Attainable Yield",
          raster_name = "a2_2050_attainable_yield.png",
          plot.legend = FALSE,
          colour_scale = mypalette_ya(length(levels(a2_2050_ya$cuts))))
kml_layer(a1b_2030_ya["cuts"],
          subfolder.name = "A1B 2030 Attainable Yield",
          layer.name = "A1B 2030 Attainable Yield",
          raster_name = "a1b_2030_attainable_yield.png",
          plot.legend = FALSE,
          colour_scale = mypalette_ya(length(levels(a1b_2030_ya$cuts))))
kml_layer(a1b_2050_ya["cuts"],
          subfolder.name = "A1B 2050 Attainable Yield",
          layer.name = "A1B 2050 Attainable Yield",
          raster_name = "a1b_2050_attainable_yield.png",
          plot.legend = FALSE,
          colour_scale = mypalette_ya(length(levels(a1b_2050_ya$cuts))))
kml_layer(b1_2030_ya["cuts"],
          subfolder.name = "B1 2030 Attainable Yield",
          layer.name = "B1 2030 Attainable Yield",
          raster_name = "b1_2030_attainable_yield.png",
          plot.legend = FALSE,
          colour_scale = mypalette_ya(length(levels(b1_2030_ya$cuts))))
kml_layer(b1_2050_ya["cuts"],
          subfolder.name = "B1 2050 Attainable Yield",
          layer.name = "B1 2050 Attainable Yield",
          raster_name = "b1_2050_attainable_yield.png",
          colour_scale = mypalette_ya(length(levels(b1_2050_ya$cuts))))
kml_close("Tanzania_attainable_yield.kml")

# reset working directory for further use with R code
setwd("../R Code")


#### The resulting KML files are now found in the KML folder and can be     ####
#### viewed using GoogleEarth                                               ####

#eos
