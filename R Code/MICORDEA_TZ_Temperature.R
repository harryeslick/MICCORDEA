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
library(extrafont)
library(plyr)
##### End libraries ####

#### Begin data import ####
TZ <- getData("GADM", country = "TZA", level = 0) # Get country outline from GADM
tmp <- stack(list.files(path = "~/tmp/2nd Batch/base dec-mar", pattern = "tmean[[:graph:]]{6}.tif$", full.names = TRUE))
tz.mask <- raster("~/Google Drive/Data/MICORDEA/GPS3 Yields/a230_att")

UP_Weather <- read.csv("UP_Weather.csv") # Generated from NASA POWER data set in IRRI Geoclimate Database, based on location of NDUAT from Willocquet et al. 2002

#### End data import ####

#### Data manipulation ####
## Crop and mask so only rice growing areas from MICORDEA are represented ##
t <- crop(tmp, tz.bb[[1]])
t <- extend(t, u)
t <- mask(t, tz.bb[[1]])
t <- crop(t, TZ)
t[t<8] <- NA # base 8
t_sum <- sum(t) # sum T for season
t_sum[t_sum>=2300] <- NA # anything about 2300 would produce yield


UP_Weather <- mutate(UP_Weather, tavg = (tmin+tmax/2))

#### Data visualisation ####

plot(t_sum)
plot(TZ, add = TRUE)
hist(t_sum)

summary(t_sum)


  
#eos
