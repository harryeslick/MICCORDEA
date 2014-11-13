##############################################################################
# title         : MICORDEA_TZ_Temperature.R;
# purpose       : extract and generate graphs and information on weather trends
#               : for publication;
# producer      : prepared by A. Sparks;
# last update   : in Los Baños, Philippines, Aug. 2014;
# inputs        : Generated weather files from C. Duku;
# outputs       : Graphs and tables;
# remarks 1     : ;
# Licence:      : GPL2;
##############################################################################

#### Begin libraries ####
library(raster)
library(ggplot2)
library(grid)
library(reshape2)
library(plyr)
##### End libraries ####

#### Begin data import ####
TZ <- getData("GADM", country = "TZA", level = 0) # Get country outline from GADM
tmp.base <- stack(list.files(path = "~/tmp/RICEPEST Data/base", pattern = "tmean[[:graph:]]{6}$", full.names = TRUE))
tmp.a250 <- stack(list.files(path = "~/tmp/RICEPEST Data/a250", pattern = "tmean[[:graph:]]{6}$", full.names = TRUE))
tmp.b150 <- stack(list.files(path = "~/tmp/RICEPEST Data/b159", pattern = "tmean[[:graph:]]{6}$", full.names = TRUE))

rad.base <- stack(list.files(path = "~/tmp/RICEPEST Data/base", pattern = "rad[[:graph:]]{6}$", full.names = TRUE))
rad.a250 <- stack(list.files(path = "~/tmp/RICEPEST Data/a250", pattern = "rad[[:graph:]]{6}$", full.names = TRUE))

UP <- read.csv("UP_Weather.csv")

#### End data import ####

#### Data manipulation ####
t <- data.frame(na.omit(values(tmp.a250)))
t <- apply(t, 2, mean)
t <- data.frame(seq(1, length(t)), t)
t <- mutate(t, cumT = cumsum(t))
colnames(t) <- c("day", "temp", "cumT")

r <- data.frame(na.omit(values(a250.rad)))
r <- apply(r, 2, mean)
r <- data.frame(seq(1, length(r)), r)
colnames(r) <- c("day", "rad")

up <- mutate(UP, tmean = tmin+tmax/2)

#### Graphing ####

a <- ggplot(data = t, aes(x = day, y = cumT))
a + geom_line()

b <- ggplot(data = r, aes(x = day, y = rad))
b + geom_line()

c <- ggplot(data = up, aes(x = wdate, y = tmean))
c + geom_line()

#eos
