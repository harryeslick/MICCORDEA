##############################################################################
# title         : MICORDEA_TZ_Yield_Loss_Results.R;
# purpose       : analyse the yield loss results from the MICORDEA project
#               : and generate plots for publication;
# producer      : prepared by A. Sparks;
# last update   : IRRI, Los Baños, March 2015;
# inputs        : ESRI files of yield losses and attainable yield for Tanzania calculated using RICEPEST;
# outputs       : Histograms and maps of yield losses for base/2030/2050 a2/b1/ab scenario;
# remarks 1     : ;
# Licence:      : GPL2;
##############################################################################

#### Begin libraries ####
library(raster)
library(ggplot2)
library(reshape)
library(extrafont)
library(maps)
library(plyr)
library(dplyr)
library(wesanderson)
library(ggthemes)
library(scales)
library(Hmisc)
##### End libraries ####

#### Begin data import ####
TZ <- getData("GADM", country = "TZA", level = 2) # Get country data from GADM

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
p.bb.loss.a230 <- mutate(p.bb.loss.a230, Change = a230_att-base_att)
p.bb.loss.a250 <- mutate(p.bb.loss.a250, Change = a250_att-base_att)
p.bb.loss.ab30 <- mutate(p.bb.loss.ab30, Change = ab30_att-base_att)
p.bb.loss.ab50 <- mutate(p.bb.loss.ab50, Change = ab50_att-base_att)
p.bb.loss.b130 <- mutate(p.bb.loss.b130, Change = b130_att-base_att)
p.bb.loss.b150 <- mutate(p.bb.loss.b150, Change = b150_att-base_att)

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
p.lb.loss.a230 <- mutate(p.lb.loss.a230, Change = a230_att-base_att)
p.lb.loss.a250 <- mutate(p.lb.loss.a250, Change = a250_att-base_att)
p.lb.loss.ab30 <- mutate(p.lb.loss.ab30, Change = ab30_att-base_att)
p.lb.loss.ab50 <- mutate(p.lb.loss.ab50, Change = ab50_att-base_att)
p.lb.loss.b130 <- mutate(p.lb.loss.b130, Change = b130_att-base_att)
p.lb.loss.b150 <- mutate(p.lb.loss.b150, Change = b150_att-base_att)


# Make appropriate column names
names(p.bb.loss.a230) <- names(p.bb.loss.a250) <- names(p.bb.loss.ab30) <- names(p.bb.loss.ab50) <- names(p.bb.loss.b130) <- names(p.bb.loss.b150) <- c("Longitude", "Latitude", "Future", "Base", "MAP")
names(p.lb.loss.a230) <- names(p.lb.loss.a250) <- names(p.lb.loss.ab30) <- names(p.lb.loss.ab50) <- names(p.lb.loss.b130) <- names(p.lb.loss.b150) <- c("Longitude", "Latitude", "Future", "Base", "MAP")

#### Create data frames for violin plots ####
ya <- mask(tz.ya, tz.bb.loss)
ya <- na.omit(unlist(values(ya)))
ya <- ya[, c(7, 1, 2, 3, 4, 5, 6)]

bb <- na.omit(unlist(values(tz.bb.loss)))
bb <- bb[, c(7, 1, 2, 3, 4, 5, 6)]

lb <- na.omit(unlist(values(tz.lb.loss)))
lb <- lb[, c(7, 1, 2, 3, 4, 5, 6)]

x <- c(rep("Base 2000", length(bb[, 1])), rep("A2 2030", length(bb[, 1])), rep("A2 2050", length(bb[, 1])), rep("A1B 2030", length(bb[, 1])), rep("A1B 2050", length(bb[, 1])), rep("B1 2030", length(bb[, 1])), rep("B1 2050", length(bb[, 1])))
scenarios <- c(rep("Base", length(bb[, 1])), rep("A2", length(bb[, 1])*2), rep("A1B", length(bb[, 1])*2), rep("B1", length(bb[, 1])*2))

ya <- as.vector(ya)
ya <- data.frame(x, scenarios, ya)
ya[, 1] <- factor(ya[, 1], as.character(ya[, 1]))

bb <- as.vector(bb)
bb <- data.frame(x, scenarios, bb)
bb[, 1] <- factor(bb[, 1], as.character(bb[, 1]))

lb <- as.vector(lb)
lb <- data.frame(x, scenarios, lb)
lb[, 1] <- factor(lb[, 1], as.character(lb[, 1]))

#### Cut data for mapping ####
p.bb.loss.a230$GROUP <- as.numeric(cut(p.bb.loss.a230$MAP, include.lowest = TRUE, breaks = c(-0.79, -0.64, -0.49, -0.34, -0.19, -0.04, 0.11, 0.26, 0.41, 0.56)))
p.bb.loss.a250$GROUP <- as.numeric(cut(p.bb.loss.a250$MAP, include.lowest = TRUE, breaks = c(-0.79, -0.64, -0.49, -0.34, -0.19, -0.04, 0.11, 0.26, 0.41, 0.56)))
p.bb.loss.ab30$GROUP <- as.numeric(cut(p.bb.loss.ab30$MAP, include.lowest = TRUE, breaks = c(-0.79, -0.64, -0.49, -0.34, -0.19, -0.04, 0.11, 0.26, 0.41, 0.56)))
p.bb.loss.ab50$GROUP <- as.numeric(cut(p.bb.loss.ab50$MAP, include.lowest = TRUE, breaks = c(-0.79, -0.64, -0.49, -0.34, -0.19, -0.04, 0.11, 0.26, 0.41, 0.56)))
p.bb.loss.b130$GROUP <- as.numeric(cut(p.bb.loss.b130$MAP, include.lowest = TRUE, breaks = c(-0.79, -0.64, -0.49, -0.34, -0.19, -0.04, 0.11, 0.26, 0.41, 0.56)))
p.bb.loss.b150$GROUP <- as.numeric(cut(p.bb.loss.b150$MAP, include.lowest = TRUE, breaks = c(-0.79, -0.64, -0.49, -0.34, -0.19, -0.04, 0.11, 0.26, 0.41, 0.56)))

p.bb.loss <- rbind(p.bb.loss.a230, p.bb.loss.a250, p.bb.loss.ab30, p.bb.loss.ab50, p.bb.loss.b130, p.bb.loss.b150)
SCENARIO <- c(rep("A2", length(p.bb.loss.a230[, 1])), 
               rep("A2", length(p.bb.loss.ab30[, 1])), 
               rep("AB", length(p.bb.loss.ab30[, 1])), 
               rep("AB", length(p.bb.loss.ab50[, 1])), 
               rep("B1", length(p.bb.loss.b130[, 1])), 
               rep("B1", length(p.bb.loss.b150[, 1])))
TIMESLICE <- c(rep(2030, length(p.bb.loss.a230[, 1])), 
               rep(2050, length(p.bb.loss.ab30[, 1])), 
               rep(2030, length(p.bb.loss.ab30[, 1])), 
               rep(2050, length(p.bb.loss.ab50[, 1])), 
               rep(2030, length(p.bb.loss.b130[, 1])), 
               rep(2050, length(p.bb.loss.b150[, 1])))

p.bb.loss <- cbind(p.bb.loss, SCENARIO, TIMESLICE)
p.bb.loss$GROUP <- as.factor(p.bb.loss$GROUP)
              
              
              
#### End data manipulation ####




#### Begin data visualisation ####


## Violin plots of yield loss by time slice and scenario

## Attainable yield
p.ya <- ggplot(ya, aes(x = x, y = ya))
p.ya <- p.ya + geom_violin(aes(colour = as.factor(scenarios), fill = as.factor(scenarios))) +
  scale_color_manual(values = wes_palette("Darjeeling2")) +
  scale_fill_manual(values = wes_palette("Darjeeling2")) + 
  labs(x = "Scenario and Time Slice", y = "Yield (tons/ha)") + 
  theme_few() +
  theme(legend.position = "none") +
  theme(axis.title.x = element_text(size = 10, family = "Helvetica"),
        axis.title.y = element_text(size = 10, angle = 90, family = "Helvetica"),
        axis.text = element_text(size = 9, family = "Helvetica"))
ggsave(filename = "Yield_Attainable_Violin.eps", path = "../LaTeX/Figures/", width = 140, height = 140, units = "mm")

## BB
p.bb <- ggplot(bb, aes(x = x, y = bb))
p.bb <- p.bb + geom_violin(aes(colour = as.factor(scenarios), fill = as.factor(scenarios))) +
  scale_color_manual(values = wes_palette("Darjeeling2")) +
  scale_fill_manual(values = wes_palette("Darjeeling2")) + 
  labs(x = "Scenario and Time Slice", y = "Yield loss (tons/ha)") + 
  theme_few() +
  theme(legend.position = "none") +
  theme(axis.title.x = element_text(size = 10, family = "Helvetica"),
        axis.title.y = element_text(size = 10, angle = 90, family = "Helvetica"),
        axis.text = element_text(size = 9, family = "Helvetica"))
ggsave(filename = "BB_Losses_Violin.eps", path = "../LaTeX/Figures/", width = 140, height = 140, units = "mm")

## LB
p.lb <- ggplot(lb, aes(x = x, y = lb))
p.lb <- p.lb + geom_violin(aes(colour = as.factor(scenarios), fill = as.factor(scenarios))) +
  scale_color_manual(values = wes_palette("Darjeeling2")) +
  scale_fill_manual(values = wes_palette("Darjeeling2")) + 
  labs(x = "Scenario and Time Slice", y = "Yield loss (tons/ha)") + 
  theme_few() +
  theme(legend.position = "none") +
  theme(axis.title.x = element_text(size = 10, family = "Helvetica"),
        axis.title.y = element_text(size = 10, angle = 90, family = "Helvetica"),
        axis.text = element_text(size = 9, family = "Helvetica"))
ggsave(filename = "LB_Losses_Violin.eps", path = "../LaTeX/Figures/", width = 140, height = 140, units = "mm")

#### Map of yield loss for BB ####

#### Bacterial Blight Change from Base to Future Time Points ####
bb.map <- ggplot(data = p.bb.loss, aes(y = Latitude, x = Longitude, fill = GROUP, colour = GROUP)) +
  geom_polygon(data = TZ, aes(x = long, y = lat, group = group), colour = "#333333", fill = "#333333") +
  geom_tile(size = 0.4) + # eliminates lines between the cell
  scale_colour_brewer(type = "div", 
                      palette = "RdYlBu",
                      labels = c(-0.79, -0.64, -0.49, -0.34, -0.19, -0.04, 0.11, 0.26, 0.41, 0.56),
                      expression(paste("t ", ha^"-1"))) +
  scale_fill_brewer(type = "div", 
                    palette = "RdYlBu",
                    labels = c(-0.79, -0.64, -0.49, -0.34, -0.19, -0.04, 0.11, 0.26, 0.41, 0.56),
                    expression(paste("t ", ha^"-1"))) +
  theme_few() +
  theme(axis.title.x = element_text(size = 6, family = "Helvetica"),
        axis.title.y = element_text(size = 6, angle = 90, family = "Helvetica"),
        axis.text = element_text(size = 7, family = "Helvetica"),
        legend.text = element_text(size = 8, family = "Helvetica"),
        legend.title = element_text(size = 8, family = "Helvetica")) +
  coord_equal() +
  facet_grid(TIMESLICE ~ SCENARIO) +
  coord_map("cylindrical") # use cylindrical projection at low latitude # use cylindrical projection at low latitude

ggsave("BB_Yield_Loss_Change.eps", path = "../Latex/figures", width = 190, units = "mm")

#### End data visualisation ####




#### Begin data values for tables and other text ####

round(summary(tz.ya), 4)
round(summary(tz.bb.loss), 4)
round(summary(tz.lb.loss), 4)

cellStats(tz.ya, stat = "mean", na.rm = TRUE)
cellStats(tz.bb.loss, stat = "mean", na.rm = TRUE)
cellStats(tz.lb.loss, stat = "mean", na.rm = TRUE)

#### End data values for tables and other text ####

#eos
