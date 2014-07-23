##############################################################################
# title         : MICORDEA_TZ_Yield_Loss_Results.R;
# purpose       : analyse the yield loss results from the MICORDEA project
#               : for publication;
# producer      : prepared by A. Sparks;
# last update   : in Los Baños, Philippines, Jul. 2014;
# inputs        : GTiff files of yield losses for Tanzania calculated using RICEPEST;
# outputs       : Histograms and maps of yield losses for base/2030/2050 a2/b1/ab scenario;
# remarks 1     : ;
# Licence:      : GPL2;
##############################################################################

#### Begin libraries ####
library(raster)
library(ggplot2)
library(reshape)
library(extrafont)
##### End libraries ####

#### Begin functions ####
TZ <- getData("GADM", country = "TZA", level = 0)
#### End libraries ####

#### Begin data import ####
tz.bb <- stack(list.files(path = "~/Google Drive/Data/MICORDEA/GPS3 Yields", pattern = "^[a,b].*bb$", full.names = TRUE))
tz.lb <- stack(list.files(path = "~/Google Drive/Data/MICORDEA/GPS3 Yields", pattern = "^[a,b].*lb$", full.names = TRUE))
tz.ya <- stack(list.files(path = "~/Google Drive/Data/MICORDEA/GPS3 Yields", pattern = "^[a,b].*att$", full.names = TRUE))

#### End data import ####

#### Begin data munging 
## Calculate the yield losses
tz.bb.loss <- (tz.ya-tz.bb)
tz.lb.loss <- (tz.ya-tz.lb)

points.tz.bb.loss <- rasterToPoints(tz.bb.loss[[1]])
points.tz.lb.loss <- rasterToPoints(tz.lb.loss)

# Make the points a dataframe for ggplot
bb.map.df <- data.frame(points.tz.bb.loss)
lb.map.df <- data.frame(points.tz.lb.loss)

#Make appropriate column headings
names(bb.map.df) <- names(lb.map.df) <- c("Longitude", "Latitude", "MAP")

## Create data frames
bb <- na.omit(data.frame(values(tz.bb.loss)))
lb <- na.omit(data.frame(values(tz.lb.loss)))

names(bb) <- names(lb) <- c("A2\n2030", "A2\n2050", "A1B\n2030", "A1B\n2050", "B1\n2030", "B1\n2050", "Base")

bb_melted <- melt(bb)
lb_melted <- melt(lb)

#### Begin data visualisation ####
## Graphs of yield loss
## BB
p.bb <- ggplot(bb_melted, aes(x = variable, y = value))
p.bb <- p.bb + geom_violin(aes(fill = variable, colour = variable)) +
  labs(x = "Scenario and Time Slice", y = "Yield loss (tons/ha)") + theme(legend.position = "none")

ggsave(filename = "BB_Losses_Violin.eps", path = "Graphics", width = 140, height = 140, units = "mm")

## LB
p.lb <- ggplot(lb_melted, aes(x = variable, y = value))
p.lb <- p.lb + geom_violin(aes(fill = variable, colour = variable)) +
  labs(x = "Scenario and Time Slice", y = "Yield loss (tons/ha)") + theme(legend.position = "none") +
  theme(axis.title.x = element_text(size = 10, family = "Helvetica"),
        axis.title.y = element_text(size = 10, angle = 90, family = "Helvetica"),
        axis.text = element_text(size = 9, family = "Helvetica"))
ggsave(filename = "LB_Losses_Violin.eps", path = "Graphics", width = 140, height = 140, units = "mm")

## Maps of yield loss
ggplot(data = bb.map.df, aes(y = Latitude, x = Longitude)) +
  geom_raster(aes(fill = MAP)) +
  coord_equal() +
  scale_fill_gradient("Tons/Ha") +
  geom_polygon(data = TZ, aes(x = long, y = lat, group = group), colour = "black", alpha = 0) # add country borders



#### End data visualisation ####

#eos
