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
##### End libraries ####

#### Begin functions ####
source("Functions/multiplot.R")
#### End libraries ####

#### Begin data import ####
tz.bb <- stack(list.files(path = "~/Google Drive/Data/MICORDEA/GPS3 Yields", pattern = "^[a,b].*bb$", full.names = TRUE))
tz.lb <- stack(list.files(path = "~/Google Drive/Data/MICORDEA/GPS3 Yields", pattern = "^[a,b].*lb$", full.names = TRUE))
tz.ya <- stack(list.files(path = "~/Google Drive/Data/MICORDEA/GPS3 Yields", pattern = "^[a,b].*att$", full.names = TRUE))

#### End data import ####

#### Begin data munging 
## Calculate the percent losses
tz.bb.loss <- (tz.ya-tz.bb)/tz.ya*100
tz.lb.loss <- (tz.ya-tz.lb)/tz.ya*100

## Create data frames
bb <- na.omit(data.frame(values(tz.bb.loss)))
lb <- na.omit(data.frame(values(tz.lb.loss)))

# A1B
bb.a1b.2030 <- data.frame(bb[, 3], bb[, 7])
names(bb.a1b.2030) <- c("A1B", "Base")
bb.a1b.2030 <- melt(bb.a1b.2030)

bb.a1b.2050 <- data.frame(bb[, 4], bb[, 7])
names(bb.a1b.2050) <- c("A1B", "Base")
bb.a1b.2050 <- melt(bb.a1b.2050)

# A2
bb.a2.2030 <- data.frame(bb[, 1], bb[, 7])
names(bb.a2.2030) <- c("A2", "Base")
bb.a2.2030 <- melt(bb.a2.2030)

bb.a2.2050 <- data.frame(bb[, 2], bb[, 7])
names(bb.a2.2050) <- c("A2", "Base")
bb.a2.2050 <- melt(bb.a2.2050)

# B1
bb.b1.2030 <- data.frame(bb[, 5], bb[, 7])
names(bb.b1.2030) <- c("B1", "Base")
bb.b1.2030 <- melt(bb.b1.2030)

bb.b1.2050 <- data.frame(bb[, 6], bb[, 7])
names(bb.b1.2050) <- c("B1", "Base")
bb.b1.2050 <- melt(bb.b1.2050)

#### Begin data visualisation ####
## A1B 2030
p.bb <- ggplot(bb.a1b.2030, aes(x = value, fill = variable, group = variable))
p.bb1 <- p.bb + stat_density(alpha = 0.4) + 
  labs(x = "", fill = "Scenario", colour = "Scenario", title = "2030")

# A1B 2050
p.bb <- ggplot(bb.a2.2050, aes(x = value, fill = variable))
p.bb2 <- p.bb + stat_density(alpha = 0.4) + 
  xlab("Percent yield loss due to Bacterial Blight") +
  labs(x = "", fill = "Scenario", colour = "Scenario", title = "2050", y = "")

## A2 2030
p.bb <- ggplot(bb.a2.2030, aes(x = value, fill = variable))
p.bb3 <- p.bb + stat_density(alpha = 0.4) + 
  xlab("Percent yield loss due\nto Bacterial Blight") +
  labs(x = "", fill = "Scenario", colour = "Scenario")

# A2 2050
p.bb <- ggplot(bb.a1b.2050, aes(x = value, fill = variable))
p.bb4 <- p.bb + stat_density(alpha = 0.4) + 
  xlab("Percent yield loss due to Bacterial Blight") +
  labs(x = "", fill = "Scenario", colour = "Scenario", y = "")

## B1
p.bb <- ggplot(bb.b1.2030, aes(x = value, fill = variable))
p.bb5 <- p.bb + stat_density(alpha = 0.4) + 
  xlab("Percent yield loss due to Bacterial Blight") + 
  labs(x = "Yield loss due\nto Bacterial Blight (%)", fill = "Scenario", colour = "Scenario")

p.bb <- ggplot(bb.b1.2050, aes(x = value, fill = variable))
p.bb6 <- p.bb + stat_density(alpha = 0.4) + 
  xlab("Percent yield loss due to Bacterial Blight") +
  labs(x = "Yield loss due\nto Bacterial Blight (%)", y = "", fill = "Scenario", colour = "Scenario")

multiplot(p.bb1, p.bb3, p.bb5, p.bb2, p.bb4, p.bb6, cols = 2)

#### End data visualisation ####

#eos
