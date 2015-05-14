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

#### End libraries ####

#### Begin data import ####

#load Raster files and set CRS
tz.bb <- stack(list.files(path = "../Data/RICEPEST Modified GPS3 Output", 
                          pattern = "^[a,b].*bb$", full.names = TRUE))
crs(tz.bb) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
tz.lb <- stack(list.files(path = "../Data/RICEPEST Modified GPS3 Output", 
                          pattern = "^[a,b].*lb$", full.names = TRUE))
crs(tz.lb) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
tz.ya <- stack(list.files(path = "../Data/RICEPEST Modified GPS3 Output", 
                          pattern = "^[a,b].*att$", full.names = TRUE))
crs(tz.ya) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

#### End data import ####

#### Begin data manipulation ####

#caclulate loss due to BB
tz.bb.loss <- (tz.ya-tz.bb)

#calculate change
for(i in 1:6){
  change <- tz.bb.loss[[i]]-tz.bb.loss[[7]]
  if(i == 1){tz.bb.change <- change} else tz.bb.change <- stack(tz.bb.change, change)
}

#convert values to classes
tz.bb.change <- cut(tz.bb.change, 
                    breaks = breaks[, 2], 
                    include.lowest = TRUE)
#reclassify BB yield loss changes for mapping purposes
tz.bb.change <- reclassify(tz.bb.change, 
                           breaks, 
                           by = breaks[, 1], 
                           which = breaks[, 2])
#### End data manipulation ####

#### Begin KML export and visualize in GoogleEarth ####

#### Set up a few items so that our KML file outputs match Figure 7 in manuscript ####

#create data frame with numbered classes and corresponding breaks
ColorBreaks <- data.frame(seq(1:6), seq(-0.79, 0.56, by = .25))
NColorBreaks <- length(ColorBreaks[, 1])-1

#set palette
mypalette <- colorRampPalette(brewer.pal(NColorBreaks, "RdYlBu"))

#assign useful names to layers for file outputs
names(tz.bb.change) <- c("a2_2030_change", "a2_2050_change", 
                         "a1b_2030_change", "a1b_2050_change",
                         "b1_2030_change", "b1_2050_change")

#### End setup ####
#create the KML files
kml_layer.Raster(tz.bb.change, 
                 colour_scale = mypalette(length(levels(as.factor(tz.bb.change[[5]])))))

#### End KML export and visualize in GoogleEarth ####

#eos