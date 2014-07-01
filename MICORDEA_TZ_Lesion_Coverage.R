##############################################################################
# title         : MICORDEA_TZ_Lesion_Coverage.R;
# purpose       : analyse the lesion coverage results from the MICORDEA project;
#               : for publication;
# producer      : prepared by A. Sparks;
# last update   : in Los Baños, Philippines, Jul. 2014;
# inputs        : GTiff files of yield losses for Tanzania calculated using RICEPEST;
# outputs       : Histograms and maps of lesion coverages for base/2030/2050 a2/b1/ab scenario;
# remarks 1     : ;
# Licence:      : GPL2;
##############################################################################

#### Begin libraries ####
library(raster)
library(ggplot2)
##### End libraries ####

#### Begin data import ####
## Leaf blast files ##
tz.base.lb <- stack(list.files(path = "~/Google Drive/Data/MICORDEA/Disease Modelling/base/", pattern = "[[:graph:]]+blast_[[:digit:]]{2,3}.tif$", full.names = TRUE)) # stack all files for blast, base time slice/scenario
tz.base.lb[tz.base.lb == -9999] <- NA # set -9999 values to NA for R

tz.2030.a2.lb <- stack(list.files(path = "~/Google Drive/Data/MICORDEA/Disease Modelling/2030/a2/", pattern = "[[:graph:]]+blast_[[:digit:]]{2,3}.tif$", full.names = TRUE)) # A2 2030
tz.2030.a2.lb[tz.2030.a2.lb == -9999] <- NA
tz.2050.a2.lb <- stack(list.files(path = "~/Google Drive/Data/MICORDEA/Disease Modelling/2050/a2/", pattern = "[[:graph:]]+blast_[[:digit:]]{2,3}.tif$", full.names = TRUE)) # A2 2050
tz.2050.a2.lb[tz.2050.a2.lb == -9999] <- NA

tz.2030.ab.lb <- stack(list.files(path = "~/Google Drive/Data/MICORDEA/Disease Modelling/2030/ab/", pattern = "[[:graph:]]+blast_[[:digit:]]{2,3}.tif$", full.names = TRUE)) # A1B 2030
tz.2030.ab.lb[tz.2030.ab.lb == -9999] <- NA
tz.2050.ab.lb <- stack(list.files(path = "~/Google Drive/Data/MICORDEA/Disease Modelling/2050/ab/", pattern = "[[:graph:]]+blast_[[:digit:]]{2,3}.tif$", full.names = TRUE)) # A1B 2050
tz.2050.ab.lb[tz.2050.ab.lb == -9999] <- NA

tz.2030.b1.lb <- stack(list.files(path = "~/Google Drive/Data/MICORDEA/Disease Modelling/2030/b1/", pattern = "[[:graph:]]+blast_[[:digit:]]{2,3}.tif$", full.names = TRUE)) # B1 2030
tz.2030.b1.lb[tz.2030.b1.lb == -9999] <- NA
tz.2050.b1.lb <- stack(list.files(path = "~/Google Drive/Data/MICORDEA/Disease Modelling/2050/b1/", pattern = "[[:graph:]]+blast_[[:digit:]]{2,3}.tif$", full.names = TRUE)) # B1 2050
tz.2050.b1.lb[tz.2050.b1.lb == -9999] <- NA

## Bacterial Blight Files ##
tz.base.bb <- stack(list.files(path = "~/Google Drive/Data/MICORDEA/Disease Modelling/base/", pattern = "[[:graph:]]+bblight_[[:digit:]]{2,3}.tif$", full.names = TRUE)) # Base
tz.base.bb[tz.base.bb == -9999] <- NA

tz.2030.a2.bb <- stack(list.files(path = "~/Google Drive/Data/MICORDEA/Disease Modelling/2030/a2/", pattern = "[[:graph:]]+bblight_[[:digit:]]{2,3}.tif$", full.names = TRUE)) # A2 2030
tz.2030.a2.bb[tz.2030.a2.bb == -9999] <- NA
tz.2050.a2.bb <- stack(list.files(path = "~/Google Drive/Data/MICORDEA/Disease Modelling/2050/a2/", pattern = "[[:graph:]]+bblight_[[:digit:]]{2,3}.tif$", full.names = TRUE)) # A2 2050
tz.2050.a2.bb[tz.2050.a2.bb == -9999] <- NA

tz.2030.ab.bb <- stack(list.files(path = "~/Google Drive/Data/MICORDEA/Disease Modelling/2030/ab/", pattern = "[[:graph:]]+bblight_[[:digit:]]{2,3}.tif$", full.names = TRUE)) # A1B 2030
tz.2030.ab.bb[tz.2030.ab.bb == -9999] <- NA
tz.2050.ab.bb <- stack(list.files(path = "~/Google Drive/Data/MICORDEA/Disease Modelling/2050/ab/", pattern = "[[:graph:]]+bblight_[[:digit:]]{2,3}.tif$", full.names = TRUE)) # A1B 2050
tz.2050.ab.bb[tz.2050.ab.bb == -9999] <- NA

tz.2030.b1.bb <- stack(list.files(path = "~/Google Drive/Data/MICORDEA/Disease Modelling/2030/b1/", pattern = "[[:graph:]]+bblight_[[:digit:]]{2,3}.tif$", full.names = TRUE)) # B1 2030
tz.2030.b1.bb[tz.2030.b1.bb == -9999] <- NA
tz.2050.b1.bb <- stack(list.files(path = "~/Google Drive/Data/MICORDEA/Disease Modelling/2050/b1/", pattern = "[[:graph:]]+bblight_[[:digit:]]{2,3}.tif$", full.names = TRUE)) # B1 2050
tz.2050.b1.bb[tz.2050.b1.bb == -9999] <- NA
#### End data import ####


#### Extract data and make dataframe for visualisation ####
lb <- data.frame(values(tz.base.lb), values(tz.2030.a2.lb), values(tz.2050.a2.lb), values(tz.2030.ab.lb), values(tz.2050.ab.lb), values(tz.2030.b1.lb), values(tz.2050.b1.lb))
bb <- data.frame(values(tz.base.bb), values(tz.2030.a2.bb), values(tz.2050.a2.bb), values(tz.2030.ab.bb), values(tz.2050.ab.bb), values(tz.2030.b1.bb), values(tz.2050.b1.bb))

## create data frame for season-long line graphs of average disease progress in Tanzania
lb.avg <- apply(lb, 2, mean, na.rm = TRUE) # create matrix of average leaf blast lesion coverage for all of Tanzania, by day of growing season
bb.avg <- apply(bb, 2, mean, na.rm = TRUE) # create matrix of average bacterial leaf blight lesion coverage for all of Tanzaina, by day of growing season

x <- c(rep("Base", 121), rep("A2", 242), rep("A1B", 242), rep("B1", 242)) # Create vector of emission scenario
y <- c(rep(2000, 121), rep(2030, 121), rep(2050, 121), rep(2030, 121), rep(2050, 121), rep(2030, 121), rep(2050, 121)) # Create vector of time-slice midpoint
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
## lb
p <- ggplot(lb.avg, aes(x = Day, y = Leaf.Blast, group = Scenario)) +
  geom_line(aes(linetype = as.factor(Scenario)), size = 1) +
  scale_x_continuous("Day of Season") + scale_y_continuous("Percent Coverage\nLeaf Blast Lesion", limits = c(0, 40)) + 
  scale_linetype_discrete("Emission\nScenario") +
  theme_bw()
p + facet_grid(. ~ Time.Slice)

q <- ggplot(bb.avg, aes(x = Day, y = Bacterial.Blight, group = Scenario)) + 
  geom_line(aes(linetype = as.factor(Scenario)), size = 1) + 
  scale_x_continuous("Day of Season") + scale_y_continuous("Percent Coverage\nLeaf Bacterial Blight Lesion") + 
  scale_linetype_discrete("Emission\nScenario") +
  theme_bw()
q + facet_grid(. ~ Time.Slice)

#### End data visualisation ####


#eos
