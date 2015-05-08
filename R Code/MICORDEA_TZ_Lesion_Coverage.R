##############################################################################
# title         : MICORDEA_TZ_Lesion_Coverage.R;
# purpose       : analyse the lesion coverage results from the MICORDEA project;
#               : for publication;
# producer      : prepared by A. Sparks;
# last update   : in Los Baños, Philippines, May 2015;
# inputs        : GTiff files of yield losses for Tanzania calculated using RICEPEST;
# outputs       : Line graphs of disease progress for each time slice and scenario;
# remarks 1     : ;
# Licence:      : GPL2;
##############################################################################

#### Begin libraries ####
library(raster)
library(ggplot2)
library(grid)
library(scales)
library(extrafont)
library(wesanderson)
library(ggthemes)
##### End libraries ####

loadfonts(device = "postscript")

#### Begin data import ####
## Leaf blast percent lesion coverage files ##
tz.base.lb <- stack(list.files(path = "../Data/EPIRICE Output/base/", 
                               pattern = "_blast_[[:digit:]]{2,3}.tif$", 
                               full.names = TRUE)) # stack all files for blast, base time slice/scenario
tz.base.lb[tz.base.lb == -9999] <- NA # set -9999 values to NA for R

tz.2030.a2.lb <- stack(list.files(path = "../Data/EPIRICE Output/2030/a2/", 
                                  pattern = "_blast_[[:digit:]]{2,3}.tif$", 
                                  full.names = TRUE)) # A2 2030
tz.2030.a2.lb[tz.2030.a2.lb == -9999] <- NA
tz.2050.a2.lb <- stack(list.files(path = "../Data/EPIRICE Output/2050/a2/", 
                                  pattern = "_blast_[[:digit:]]{2,3}.tif$", 
                                  full.names = TRUE)) # A2 2050
tz.2050.a2.lb[tz.2050.a2.lb == -9999] <- NA

tz.2030.ab.lb <- stack(list.files(path = "../Data/EPIRICE Output/2030/ab/", 
                                  pattern = "_blast_[[:digit:]]{2,3}.tif$", 
                                  full.names = TRUE)) # A1B 2030
tz.2030.ab.lb[tz.2030.ab.lb == -9999] <- NA
tz.2050.ab.lb <- stack(list.files(path = "../Data/EPIRICE Output/2050/ab/", 
                                  pattern = "_blast_[[:digit:]]{2,3}.tif$", 
                                  full.names = TRUE)) # A1B 2050
tz.2050.ab.lb[tz.2050.ab.lb == -9999] <- NA

tz.2030.b1.lb <- stack(list.files(path = "../Data/EPIRICE Output/2030/b1/", 
                                  pattern = "_blast_[[:digit:]]{2,3}.tif$", 
                                  full.names = TRUE)) # B1 2030
tz.2030.b1.lb[tz.2030.b1.lb == -9999] <- NA
tz.2050.b1.lb <- stack(list.files(path = "../Data/EPIRICE Output/2050/b1/", 
                                  pattern = "_blast_[[:digit:]]{2,3}.tif$", 
                                  full.names = TRUE)) # B1 2050
tz.2050.b1.lb[tz.2050.b1.lb == -9999] <- NA

## Bacterial blight percent lesion coverage files ##
tz.base.bb <- stack(list.files(path = "../Data/EPIRICE Output/base/", 
                               pattern = "_bblight_[[:digit:]]{2,3}.tif$",
                               full.names = TRUE)) # Base
tz.base.bb[tz.base.bb == -9999] <- NA

tz.2030.a2.bb <- stack(list.files(path = "../Data/EPIRICE Output/2030/a2/", 
                                  pattern = "_bblight_[[:digit:]]{2,3}.tif$", 
                                  full.names = TRUE)) # A2 2030
tz.2030.a2.bb[tz.2030.a2.bb == -9999] <- NA
tz.2050.a2.bb <- stack(list.files(path = "../Data/EPIRICE Output/2050/a2/", 
                                  pattern = "_bblight_[[:digit:]]{2,3}.tif$", 
                                  full.names = TRUE)) # A2 2050
tz.2050.a2.bb[tz.2050.a2.bb == -9999] <- NA

tz.2030.ab.bb <- stack(list.files(path = "../Data/EPIRICE Output/2030/ab/", 
                                  pattern = "_bblight_[[:digit:]]{2,3}.tif$", 
                                  full.names = TRUE)) # A1B 2030
tz.2030.ab.bb[tz.2030.ab.bb == -9999] <- NA
tz.2050.ab.bb <- stack(list.files(path = "../Data/EPIRICE Output/2050/ab/", 
                                  pattern = "_bblight_[[:digit:]]{2,3}.tif$", 
                                  full.names = TRUE)) # A1B 2050
tz.2050.ab.bb[tz.2050.ab.bb == -9999] <- NA

tz.2030.b1.bb <- stack(list.files(path = "../Data/EPIRICE Output/2030/b1/", 
                                  pattern = "_bblight_[[:digit:]]{2,3}.tif$", 
                                  full.names = TRUE)) # B1 2030
tz.2030.b1.bb[tz.2030.b1.bb == -9999] <- NA
tz.2050.b1.bb <- stack(list.files(path = "../Data/EPIRICE Output/2050/b1/",
                                  pattern = "_bblight_[[:digit:]]{2,3}.tif$",
                                  full.names = TRUE)) # B1 2050
tz.2050.b1.bb[tz.2050.b1.bb == -9999] <- NA
#### End data import ####


#### Extract data and make dataframe for visualisation ####
lb <- data.frame(values(tz.base.lb), 
                 values(tz.2030.a2.lb),
                 values(tz.2050.a2.lb),
                 values(tz.2030.ab.lb),
                 values(tz.2050.ab.lb), 
                 values(tz.2030.b1.lb), 
                 values(tz.2050.b1.lb))
bb <- data.frame(values(tz.base.bb), 
                 values(tz.2030.a2.bb),
                 values(tz.2050.a2.bb), 
                 values(tz.2030.ab.bb),
                 values(tz.2050.ab.bb), 
                 values(tz.2030.b1.bb), 
                 values(tz.2050.b1.bb))

## create data frame for season-long line graphs of average disease progress in Tanzania
lb.avg <- apply(lb, 2, mean, na.rm = TRUE) # create matrix of average leaf blast lesion coverage for all of Tanzania, by day of growing season
bb.avg <- apply(bb, 2, mean, na.rm = TRUE) # create matrix of average bacterial leaf blight lesion coverage for all of Tanzaina, by day of growing season

x <- c(rep("Base", 121), 
       rep("A2", 242),
       rep("A1B", 242), 
       rep("B1", 242)) # Create vector of emission scenario
y <- c(rep(2000, 121), 
       rep(2030, 121), 
       rep(2050, 121), 
       rep(2030, 121), 
       rep(2050, 121),
       rep(2030, 121), 
       rep(2050, 121)) # Create vector of time-slice midpoint
z <- c(as.numeric(rep(1:121, 7))) # Create vector of days in growing season
z <- c(as.numeric(rep(1:121, 7))) # Create vector of days in growing season

lb.avg <- data.frame(x, y, z, lb.avg) # Combind the vectors into one dataframe for ggplot2
bb.avg <- data.frame(x, y, z, bb.avg)

names(lb.avg) <- c("Scenario", "Time.Slice", "Day", "Leaf.Blast") # Assign names to leaf blast data frame
names(bb.avg) <- c("Scenario", "Time.Slice", "Day", "Bacterial.Blight") # Assign names to bacterial blight data frame
row.names(lb.avg) <- row.names(bb.avg) <- 1:nrow(lb.avg) # Assign sensible row names for datafraem

lb.avg <- subset(lb.avg, Day > 21) # EPIRICE begins at day 20 of the simulation, this drops all prior values
bb.avg <- subset(bb.avg, Day > 21) 

#### End extract data and make dataframe for visualisation ####

#### Begin data visualisation ####
## Leaf blast graph ##

figure.2 <- ggplot(lb.avg, aes(x = Day, y = Leaf.Blast, group = Scenario)) +
  geom_line(aes(colour = as.factor(Scenario), linetype = as.factor(Scenario)), size = 1) +
  scale_x_continuous("Day of Season") + scale_y_continuous("Leaf Coverage by Bacterial Blight Lesions (%)", limits = c(0, 40)) + 
  scale_linetype_discrete("Emission\nScenario") +
  scale_colour_manual("Emission\nScenario", values = wes_palette("Moonrise3")) +
  theme_few() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.title.x = element_text(size = 10, family = "Helvetica"),
        axis.title.y = element_text(size = 10, angle = 90, family = "Helvetica"),
        legend.position = c(0.1, 0.825), 
        legend.background = element_rect(fill = "white", colour = "black"),
        legend.key.width = unit(6, "mm"),
        legend.text = element_text(size = 8, family = "Helvetica"),
        plot.margin = unit(c(.5, .5, .5, .5), "lines")) +
  facet_grid(. ~ Time.Slice)
ggsave("../Latex/Figures/LB.eps", width = 140, height = 140, units = "mm")

## Bacterial blight graph ##

figure.3 <- ggplot(bb.avg, aes(x = Day, y = Bacterial.Blight, group = Scenario)) + 
  geom_line(aes(colour = as.factor(Scenario), linetype = as.factor(Scenario)), size = 1) +
  scale_x_continuous("Day of Season") + scale_y_continuous("Leaf Coverage by Bacterial Blight Lesions (%)", limits = c(0, 40)) + 
  scale_linetype_discrete("Emission\nScenario") +
  scale_colour_manual("Emission\nScenario", values = wes_palette("Moonrise3")) +
  theme_few() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.title.x = element_text(size = 10, family = "Helvetica"),
        axis.title.y = element_text(size = 10, angle = 90, family = "Helvetica"),
        legend.position = c(0.1, 0.825), 
        legend.background = element_rect(fill = "white", colour = "black"),
        legend.key.width = unit(6, "mm"),
        legend.text = element_text(size = 8, family = "Helvetica"),
        plot.margin = unit(c(.5, .5, .5, .5), "lines")) +
  facet_grid(. ~ Time.Slice)

ggsave("../LaTeX/Figures/BB.eps", width = 140, height = 140, units = "mm")

#### End data visualisation ####

#eos
