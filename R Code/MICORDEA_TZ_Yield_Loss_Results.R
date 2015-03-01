##############################################################################
# title         : MICORDEA_TZ_Yield_Loss_Results.R;
# purpose       : analyse the yield loss results from the MICORDEA project
#               : and generate plots for publication;
# producer      : prepared by A. Sparks;
# last update   : in Los Baños, Philippines, March 2015;
# inputs        : ESRI files of yield losses and attainable yield for Tanzania calculated using RICEPEST;
# outputs       : Violin plots and maps of yield losses for base/2030/2050 a2/b1/ab scenario;
# remarks 1     : ;
# Licence:      : GPL2;
##############################################################################

#### Begin libraries ####
library(raster)
library(ggplot2)
library(reshape)
library(extrafont)
library(maps)
library(dplyr)
library(plyr)
##### End libraries ####

#### Begin data import ####
TZ <- getData("GADM", country = "TZA", level = 0) # Get country outline from GADM

tz.bb <- stack(list.files(path = "~/Google Drive/Data/MICORDEA/Modified GPS3 Results", pattern = "^[a,b].*bb$", full.names = TRUE))
tz.lb <- stack(list.files(path = "~/Google Drive/Data/MICORDEA/Modified GPS3 Results", pattern = "^[a,b].*lb$", full.names = TRUE))
tz.ya <- stack(list.files(path = "~/Google Drive/Data/MICORDEA/Modified GPS3 Results", pattern = "^[a,b].*att$", full.names = TRUE))

#### End data import ####




#### Begin data manipulation ####

## Calculate the yield losses
tz.bb.loss <- (tz.ya-tz.bb)
tz.lb.loss <- (tz.ya-tz.lb)

## BB
#Create data frames from rasters
p.bb.loss.a230 <- data.frame(rasterToPoints(tz.bb.loss[[1]]))
p.bb.loss.a250 <- data.frame(rasterToPoints(tz.bb.loss[[2]]))
p.bb.loss.ab30 <- data.frame(rasterToPoints(tz.bb.loss[[3]]))
p.bb.loss.ab50 <- data.frame(rasterToPoints(tz.bb.loss[[4]]))
p.bb.loss.b130 <- data.frame(rasterToPoints(tz.bb.loss[[5]]))
p.bb.loss.b150 <- data.frame(rasterToPoints(tz.bb.loss[[6]]))
p.bb.loss.base <- data.frame(rasterToPoints(tz.bb.loss[[7]]))

#Join data frames (future and base)
p.bb.loss.a230 <- inner_join(p.bb.loss.a230, p.bb.loss.base)
p.bb.loss.a250 <- inner_join(p.bb.loss.a250, p.bb.loss.base)
p.bb.loss.ab30 <- inner_join(p.bb.loss.ab30, p.bb.loss.base)
p.bb.loss.ab50 <- inner_join(p.bb.loss.ab50, p.bb.loss.base)
p.bb.loss.b130 <- inner_join(p.bb.loss.b130, p.bb.loss.base)
p.bb.loss.b150 <- inner_join(p.bb.loss.b150, p.bb.loss.base)

#Calculate change in loss
p.bb.loss.a230 <- mutate(p.bb.loss.a230, Change = (a230_att-base_att))
p.bb.loss.a250 <- mutate(p.bb.loss.a250, Change = (a250_att-base_att))
p.bb.loss.ab30 <- mutate(p.bb.loss.ab30, Change = (ab30_att-base_att))
p.bb.loss.ab50 <- mutate(p.bb.loss.ab50, Change = (ab50_att-base_att))
p.bb.loss.b130 <- mutate(p.bb.loss.b130, Change = (b130_att-base_att))
p.bb.loss.b150 <- mutate(p.bb.loss.b150, Change = (b150_att-base_att))

## LB
#Create data frames from rasters
p.lb.loss.a230 <- data.frame(rasterToPoints(tz.lb.loss[[1]]))
p.lb.loss.a250 <- data.frame(rasterToPoints(tz.lb.loss[[2]]))
p.lb.loss.ab30 <- data.frame(rasterToPoints(tz.lb.loss[[3]]))
p.lb.loss.ab50 <- data.frame(rasterToPoints(tz.lb.loss[[4]]))
p.lb.loss.b130 <- data.frame(rasterToPoints(tz.lb.loss[[5]]))
p.lb.loss.b150 <- data.frame(rasterToPoints(tz.lb.loss[[6]]))
p.lb.loss.base <- data.frame(rasterToPoints(tz.lb.loss[[7]]))

#Join data frames (future and base)
p.lb.loss.a230 <- inner_join(p.lb.loss.a230, p.lb.loss.base)
p.lb.loss.a250 <- inner_join(p.lb.loss.a250, p.lb.loss.base)
p.lb.loss.ab30 <- inner_join(p.lb.loss.ab30, p.lb.loss.base)
p.lb.loss.ab50 <- inner_join(p.lb.loss.ab50, p.lb.loss.base)
p.lb.loss.b130 <- inner_join(p.lb.loss.b130, p.lb.loss.base)
p.lb.loss.b150 <- inner_join(p.lb.loss.b150, p.lb.loss.base)

#Calculate change in loss
p.lb.loss.a230 <- mutate(p.lb.loss.a230, Change = (a230_att-base_att))
p.lb.loss.a250 <- mutate(p.lb.loss.a250, Change = (a250_att-base_att))
p.lb.loss.ab30 <- mutate(p.lb.loss.ab30, Change = (ab30_att-base_att))
p.lb.loss.ab50 <- mutate(p.lb.loss.ab50, Change = (ab50_att-base_att))
p.lb.loss.b130 <- mutate(p.lb.loss.b130, Change = (b130_att-base_att))
p.lb.loss.b150 <- mutate(p.lb.loss.b150, Change = (b150_att-base_att))


# Make appropriate column names
names(p.bb.loss.a230) <- names(p.bb.loss.a250) <- names(p.bb.loss.ab30) <- names(p.bb.loss.ab50) <- names(p.bb.loss.b130) <- names(p.bb.loss.b150) <- c("Longitude", "Latitude", "Future", "Base", "MAP")
names(p.lb.loss.a230) <- names(p.lb.loss.a250) <- names(p.lb.loss.ab30) <- names(p.lb.loss.ab50) <- names(p.lb.loss.b130) <- names(p.lb.loss.b150) <- c("Longitude", "Latitude", "Future", "Base", "MAP")

#### Create data frames for violin plots ####
bb <- na.omit(data.frame(values(tz.bb.loss)))
lb <- na.omit(data.frame(values(tz.lb.loss)))
ya <- na.omit(data.frame(values(tz.ya)))

names(bb) <- names(lb) <- c("A2\n2030", "A2\n2050", "A1B\n2030", "A1B\n2050", "B1\n2030", "B1\n2050", "Base")

ya_melted <- melt(ya)
bb_melted <- melt(bb)
lb_melted <- melt(lb)

#### End data manipulation ####




#### Begin data visualisation ####


## Violin plots of yield loss by time slice and scenario

## Attainable yield
p.ya <- ggplot(ya_melted, aes(x = variable, y = value))
p.ya <- p.ya + geom_violin(aes(fill = variable, colour = variable)) +
  labs(x = "Scenario and Time Slice", y = "Yield loss (tons/ha)") + theme(legend.position = "none")
ggsave(filename = "Attainable_Yield_Violin.eps", path = "Graphics", width = 140, height = 140, units = "mm")

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




#### Maps of yield loss ####


#### Bacterial Blight Change from Base to Future Time Points ####
bb.map1 <- ggplot(data = p.bb.loss.a230, aes(y = Latitude, x = Longitude, fill = MAP, colour = MAP)) +
  geom_polygon(data = TZ, aes(x = long, y = lat, group = group), colour = "#333333", fill = "#333333") +
  geom_tile(size = 0.4) + # eliminates lines between the cell
  scale_fill_gradient2(low = "#0000FF", mid = "#FFFFFF", high ="#FF0000", midpoint = 0, space = "rgb", guide = "colourbar", limits = c(-0.25, 0.31), "Tons/Ha") +
  scale_colour_gradient2(low = "#0000FF", mid = "#FFFFFF", high ="#FF0000", midpoint = 0, space = "rgb", guide = "colourbar", limits = c(-0.25, 0.31), "Tons/Ha") +
  theme(plot.title = element_text(face = "bold", family = "Helvetica"), 
        axis.title.y = element_text(size = 10, angle = 90, family = "Helvetica"),
        axis.title.x = element_text(size = 10, family = "Helvetica"),
        axis.text = element_text(size = 9, family = "Helvetica")) +
  coord_equal() +
  coord_map("cylindrical") # use cylindrical projection at low latitude

ggsave("A2 2030 BB Change.eps", path = "Graphics", width = 140, height = 140, units = "mm")

bb.map2 <- ggplot(data = p.bb.loss.a250, aes(y = Latitude, x = Longitude, fill = MAP, colour = MAP)) +
  geom_polygon(data = TZ, aes(x = long, y = lat, group = group), colour = "#333333", fill = "#333333") +
  geom_tile(size = 0.4) + # eliminates lines between the cell
  scale_fill_gradient2(low = "#0000FF", mid = "#FFFFFF", high ="#FF0000", midpoint = 0, space = "rgb", guide = "colourbar", limits = c(-0.25, 0.31), "Tons/Ha") +
  scale_colour_gradient2(low = "#0000FF", mid = "#FFFFFF", high ="#FF0000", midpoint = 0, space = "rgb", guide = "colourbar", limits = c(-0.25, 0.31), "Tons/Ha") +
  theme(plot.title = element_text(face = "bold", family = "Helvetica"), 
        axis.title.y = element_text(size = 10, angle = 90, family = "Helvetica"),
        axis.title.x = element_text(size = 10, family = "Helvetica"),
        axis.text = element_text(size = 9, family = "Helvetica")) +
  coord_equal() +
  coord_map("cylindrical") # use cylindrical projection at low latitude

ggsave("A2 2050 BB Change.eps", path = "Graphics", width = 140, height = 140, units = "mm")

bb.map3 <- ggplot(data = p.bb.loss.ab30, aes(y = Latitude, x = Longitude, fill = MAP, colour = MAP)) +
  geom_polygon(data = TZ, aes(x = long, y = lat, group = group), colour = "#333333", fill = "#333333") +
  geom_tile(size = 0.4) + # eliminates lines between the cell
  scale_fill_gradient2(low = "#0000FF", mid = "#FFFFFF", high ="#FF0000", midpoint = 0, space = "rgb", guide = "colourbar", limits = c(-0.25, 0.31), "Tons/Ha") +
  scale_colour_gradient2(low = "#0000FF", mid = "#FFFFFF", high ="#FF0000", midpoint = 0, space = "rgb", guide = "colourbar", limits = c(-0.25, 0.31), "Tons/Ha") +
  theme(plot.title = element_text(face = "bold", family = "Helvetica"), 
        axis.title.y = element_text(size = 10, angle = 90, family = "Helvetica"),
        axis.title.x = element_text(size = 10, family = "Helvetica"),
        axis.text = element_text(size = 9, family = "Helvetica")) +
  coord_equal() +
  coord_map("cylindrical") # use cylindrical projection at low latitude

ggsave("A1B 2030 BB Change.eps", path = "Graphics", width = 140, height = 140, units = "mm")

bb.map4 <- ggplot(data = p.bb.loss.ab50, aes(y = Latitude, x = Longitude, fill = MAP, colour = MAP)) +
  geom_polygon(data = TZ, aes(x = long, y = lat, group = group), colour = "#333333", fill = "#333333") +
  geom_tile(size = 0.4) + # eliminates lines between the cell
  scale_fill_gradient2(low = "#0000FF", mid = "#FFFFFF", high ="#FF0000", midpoint = 0, space = "rgb", guide = "colourbar", limits = c(-0.25, 0.31), "Tons/Ha") +
  scale_colour_gradient2(low = "#0000FF", mid = "#FFFFFF", high ="#FF0000", midpoint = 0, space = "rgb", guide = "colourbar", limits = c(-0.25, 0.31), "Tons/Ha") +
  theme(plot.title = element_text(face = "bold", family = "Helvetica"), 
        axis.title.y = element_text(size = 10, angle = 90, family = "Helvetica"),
        axis.title.x = element_text(size = 10, family = "Helvetica"),
        axis.text = element_text(size = 9, family = "Helvetica")) +
  coord_equal() +
  coord_map("cylindrical") # use cylindrical projection at low latitude

ggsave("A1B 2050 BB Change.eps", path = "Graphics", width = 140, height = 140, units = "mm")

bb.map5 <- ggplot(data = p.bb.loss.b130, aes(y = Latitude, x = Longitude, fill = MAP, colour = MAP)) +
  geom_polygon(data = TZ, aes(x = long, y = lat, group = group), colour = "#333333", fill = "#333333") +
  geom_tile(size = 0.4) + # eliminates lines between the cell
  scale_fill_gradient2(low = "#0000FF", mid = "#FFFFFF", high ="#FF0000", midpoint = 0, space = "rgb", guide = "colourbar", limits = c(-0.25, 0.31), "Tons/Ha") +
  scale_colour_gradient2(low = "#0000FF", mid = "#FFFFFF", high ="#FF0000", midpoint = 0, space = "rgb", guide = "colourbar", limits = c(-0.25, 0.31), "Tons/Ha") +
  theme(plot.title = element_text(face = "bold", family = "Helvetica"), 
        axis.title.y = element_text(size = 10, angle = 90, family = "Helvetica"),
        axis.title.x = element_text(size = 10, family = "Helvetica"),
        axis.text = element_text(size = 9, family = "Helvetica")) +
  coord_equal() +
  coord_map("cylindrical") # use cylindrical projection at low latitude

ggsave("B1 2030 BB Change.eps", path = "Graphics", width = 140, height = 140, units = "mm")

bb.map6 <- ggplot(data = p.bb.loss.b150, aes(y = Latitude, x = Longitude, fill = MAP, colour = MAP)) +
  geom_polygon(data = TZ, aes(x = long, y = lat, group = group), colour = "#333333", fill = "#333333") +
  geom_tile(size = 0.4) + # eliminates lines between the cell
  scale_fill_gradient2(low = "#0000FF", mid = "#FFFFFF", high ="#FF0000", midpoint = 0, space = "rgb", guide = "colourbar", limits = c(-0.25, 0.31), "Tons/Ha") +
  scale_colour_gradient2(low = "#0000FF", mid = "#FFFFFF", high ="#FF0000", midpoint = 0, space = "rgb", guide = "colourbar", limits = c(-0.25, 0.31), "Tons/Ha") +
  theme(plot.title = element_text(face = "bold", family = "Helvetica"), 
        axis.title.y = element_text(size = 10, angle = 90, family = "Helvetica"),
        axis.title.x = element_text(size = 10, family = "Helvetica"),
        axis.text = element_text(size = 9, family = "Helvetica")) +
  coord_equal() +
  coord_map("cylindrical") # use cylindrical projection at low latitude

ggsave("B1 2050 BB Change.eps", path = "Graphics", width = 140, height = 140, units = "mm")





#### Leaf Blast Change from Base to Future Time Points ####


LB.map1 <- ggplot(data = p.lb.loss.a230, aes(y = Latitude, x = Longitude, fill = MAP, colour = MAP)) +
  geom_polygon(data = TZ, aes(x = long, y = lat, group = group), colour = "#333333", fill = "#333333") +
  geom_tile(size = 0.4) + # eliminates lines between the cell
  scale_fill_gradient2(low = "#0000FF", mid = "#FFFFFF", high ="#FF0000", midpoint = 0, space = "rgb", guide = "colourbar", limits = c(-0.25, 0.31), "Tons/Ha") +
  scale_colour_gradient2(low = "#0000FF", mid = "#FFFFFF", high ="#FF0000", midpoint = 0, space = "rgb", guide = "colourbar", limits = c(-0.25, 0.31), "Tons/Ha") +
  theme(plot.title = element_text(face = "bold", family = "Helvetica"), 
        axis.title.y = element_text(size = 10, angle = 90, family = "Helvetica"),
        axis.title.x = element_text(size = 10, family = "Helvetica"),
        axis.text = element_text(size = 9, family = "Helvetica")) +
  coord_equal() +
  coord_map("cylindrical") # use cylindrical projection at low latitude

ggsave("A2 2030 LB Change.eps", path = "Graphics", width = 140, height = 140, units = "mm")

LB.map2 <- ggplot(data = p.lb.loss.a250, aes(y = Latitude, x = Longitude, fill = MAP, colour = MAP)) +
  geom_polygon(data = TZ, aes(x = long, y = lat, group = group), colour = "#333333", fill = "#333333") +
  geom_tile(size = 0.4) + # eliminates lines between the cell
  scale_fill_gradient2(low = "#0000FF", mid = "#FFFFFF", high ="#FF0000", midpoint = 0, space = "rgb", guide = "colourbar", limits = c(-0.25, 0.31), "Tons/Ha") +
  scale_colour_gradient2(low = "#0000FF", mid = "#FFFFFF", high ="#FF0000", midpoint = 0, space = "rgb", guide = "colourbar", limits = c(-0.25, 0.31), "Tons/Ha") +
  theme(plot.title = element_text(face = "bold", family = "Helvetica"), 
        axis.title.y = element_text(size = 10, angle = 90, family = "Helvetica"),
        axis.title.x = element_text(size = 10, family = "Helvetica"),
        axis.text = element_text(size = 9, family = "Helvetica")) +
  coord_equal() +
  coord_map("cylindrical") # use cylindrical projection at low latitude

ggsave("A2 2050 LB Change.eps", path = "Graphics", width = 140, height = 140, units = "mm")

LB.map3 <- ggplot(data = p.lb.loss.ab30, aes(y = Latitude, x = Longitude, fill = MAP, colour = MAP)) +
  geom_polygon(data = TZ, aes(x = long, y = lat, group = group), colour = "#333333", fill = "#333333") +
  geom_tile(size = 0.4) + # eliminates lines between the cell
  scale_fill_gradient2(low = "#0000FF", mid = "#FFFFFF", high ="#FF0000", midpoint = 0, space = "rgb", guide = "colourbar", limits = c(-0.25, 0.31), "Tons/Ha") +
  scale_colour_gradient2(low = "#0000FF", mid = "#FFFFFF", high ="#FF0000", midpoint = 0, space = "rgb", guide = "colourbar", limits = c(-0.25, 0.31), "Tons/Ha") +
  theme(plot.title = element_text(face = "bold", family = "Helvetica"), 
        axis.title.y = element_text(size = 10, angle = 90, family = "Helvetica"),
        axis.title.x = element_text(size = 10, family = "Helvetica"),
        axis.text = element_text(size = 9, family = "Helvetica")) +
  coord_equal() +
  coord_map("cylindrical") # use cylindrical projection at low latitude

ggsave("A1B 2030 LB Change.eps", path = "Graphics", width = 140, height = 140, units = "mm")

LB.map4 <- ggplot(data = p.lb.loss.ab50, aes(y = Latitude, x = Longitude, fill = MAP, colour = MAP)) +
  geom_polygon(data = TZ, aes(x = long, y = lat, group = group), colour = "#333333", fill = "#333333") +
  geom_tile(size = 0.4) + # eliminates lines between the cell
  scale_fill_gradient2(low = "#0000FF", mid = "#FFFFFF", high ="#FF0000", midpoint = 0, space = "rgb", guide = "colourbar", limits = c(-0.25, 0.31), "Tons/Ha") +
  scale_colour_gradient2(low = "#0000FF", mid = "#FFFFFF", high ="#FF0000", midpoint = 0, space = "rgb", guide = "colourbar", limits = c(-0.25, 0.31), "Tons/Ha") +
  theme(plot.title = element_text(face = "bold", family = "Helvetica"), 
        axis.title.y = element_text(size = 10, angle = 90, family = "Helvetica"),
        axis.title.x = element_text(size = 10, family = "Helvetica"),
        axis.text = element_text(size = 9, family = "Helvetica")) +
  coord_equal() +
  coord_map("cylindrical") # use cylindrical projection at low latitude

ggsave("A1B 2050 LB Change.eps", path = "Graphics", width = 140, height = 140, units = "mm")

LB.map5 <- ggplot(data = p.lb.loss.b130, aes(y = Latitude, x = Longitude, fill = MAP, colour = MAP)) +
  geom_polygon(data = TZ, aes(x = long, y = lat, group = group), colour = "#333333", fill = "#333333") +
  geom_tile(size = 0.4) + # eliminates lines between the cell
  scale_fill_gradient2(low = "#0000FF", mid = "#FFFFFF", high ="#FF0000", midpoint = 0, space = "rgb", guide = "colourbar", limits = c(-0.25, 0.31), "Tons/Ha") +
  scale_colour_gradient2(low = "#0000FF", mid = "#FFFFFF", high ="#FF0000", midpoint = 0, space = "rgb", guide = "colourbar", limits = c(-0.25, 0.31), "Tons/Ha") +
  theme(plot.title = element_text(face = "bold", family = "Helvetica"), 
        axis.title.y = element_text(size = 10, angle = 90, family = "Helvetica"),
        axis.title.x = element_text(size = 10, family = "Helvetica"),
        axis.text = element_text(size = 9, family = "Helvetica")) +
  coord_equal() +
  coord_map("cylindrical") # use cylindrical projection at low latitude

ggsave("B1 2030 LB Change.eps", path = "Graphics", width = 140, height = 140, units = "mm")

LB.map6 <- ggplot(data =p.lb.loss.b150, aes(y = Latitude, x = Longitude, fill = MAP, colour = MAP)) +
  geom_polygon(data = TZ, aes(x = long, y = lat, group = group), colour = "#333333", fill = "#333333") +
  geom_tile(size = 0.4) + # eliminates lines between the cell
  scale_fill_gradient2(low = "#0000FF", mid = "#FFFFFF", high ="#FF0000", midpoint = 0, space = "rgb", guide = "colourbar", limits = c(-0.25, 0.31), "Tons/Ha") +
  scale_colour_gradient2(low = "#0000FF", mid = "#FFFFFF", high ="#FF0000", midpoint = 0, space = "rgb", guide = "colourbar", limits = c(-0.25, 0.31), "Tons/Ha") +
  theme(plot.title = element_text(face = "bold", family = "Helvetica"), 
        axis.title.y = element_text(size = 10, angle = 90, family = "Helvetica"),
        axis.title.x = element_text(size = 10, family = "Helvetica"),
        axis.text = element_text(size = 9, family = "Helvetica")) +
  coord_equal() +
  coord_map("cylindrical") # use cylindrical projection at low latitude

ggsave("B1 2050 LB Change.eps", path = "Graphics", width = 140, height = 140, units = "mm")
                                                                                                            
#### End data visualisation ####




#### Begin data values for tables and other text ####

summary(ya)
round(summary(tz.bb.loss), 2)
round(summary(tz.lb.loss), 2)

#### End data values for tables and other text ####

#eos
