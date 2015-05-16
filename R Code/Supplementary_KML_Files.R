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
  if(i == 1){tz.bb.change <- change} else 
    tz.bb.change <- stack(tz.bb.change, change)
}

#### Convert values to classes ####
#create data frame with numbered classes and corresponding breaks
breaks <- data.frame(seq(1:7), seq(-0.79, 0.71, by = .25))

tz.bb.change <- cut(tz.bb.change, 
                    breaks = breaks[, 2], 
                    include.lowest = TRUE)

#reclassify BB yield loss changes for mapping purposes
tz.bb.change <- reclassify(tz.bb.change, 
                           breaks, 
                           by = breaks[, 1], 
                           which = breaks[, 2])

#add a Raster Atribute Table and define the raster as categorical data

a2.2030.change <- ratify(tz.bb.change[[1]])
a2.2050.change <- ratify(tz.bb.change[[2]])
a1b.2030.change <- ratify(tz.bb.change[[3]])
a1b.2050.change <- ratify(tz.bb.change[[4]])
b1.2030.change <- ratify(tz.bb.change[[5]])
b1.2050.change <- ratify(tz.bb.change[[6]])

a2.2030.rat <- levels(a2.2030.change)[[1]]
a2.2050.rat <- levels(a2.2050.change)[[2]]
a1b.2030.rat <- levels(a1b.2030.change)[[3]]
a1b.2050.rat <- levels(a1b.2050.change)[[4]]
b1.2030.rat <- levels(b1.2030.change)[[5]]
b1.2050.rat <- levels(b1.2050.change)[[6]]

classes <- c("-0.79, -0.54", 
             "-0.54, -0.29", 
             "-0.29, -0.04", 
             "-0.04, 0.21", 
             "0.21, 0.46",
             "0.46, 0.71")

a2.2030.rat$classes <-
  a2.2050.rat$classes <-
  a1b.2030.rat$classes <-
  a1b.2050.rat$classes <-
  b1.2030.rat$classes <-
  b1.2050.rat$classes <- c("-0.79, -0.54", "-0.54, -0.29", "-0.29, -0.04", "-0.04, 0.21", "0.21, 0.46", "0.46, 0.71")

levels(landClass) <- rat

levelplot(landClass, col.regions=terrain_hcl(4))

#reclassify BB yield loss changes for mapping purposes
tz.bb.change <- reclassify(tz.bb.change, 
                           breaks, 
                           by = breaks[, 1], 
                           which = breaks[, 2])
#### End data manipulation ####

#### Begin KML export and visualize in GoogleEarth ####
##set up a few items so that our KML file outputs match Figure 7 in manuscript



## Configure the RAT: first create a RAT data.frame using the
## levels method; second, set the values for each class (to be
## used by levelplot); third, assign this RAT to the raster
## using again levels


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