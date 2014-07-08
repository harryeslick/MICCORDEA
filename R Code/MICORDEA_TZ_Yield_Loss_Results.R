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
##### End libraries ####

#### Begin data import ####
tz.bb <- stack(list.files(path = "~/Google Drive/Data/MICORDEA/Results/GPS3 Final Yields tiff", pattern = "a[[:graph:]]{3}_bb1.tif$", full.names = TRUE))
tz.lb <- stack(list.files(path = "~/Google Drive/Data/MICORDEA/Results/GPS3 Final Yields tiff", pattern = "a[[:graph:]]{3}_lb1.tif$", full.names = TRUE))

## 2050 B1 extent is off, so we have to load and align those seperately
h <- raster("~/Google Drive/Data/MICORDEA/Results/GPS3 Final Yields tiff/b130_bb1.tif")
i <- raster("~/Google Drive/Data/MICORDEA/Results/GPS3 Final Yields tiff/b130_lb1.tif")

j <- raster("~/Google Drive/Data/MICORDEA/Results/GPS3 Final Yields tiff/b150_bb1.tif")
k <- raster("~/Google Drive/Data/MICORDEA/Results/GPS3 Final Yields tiff/b150_lb1.tif")

j <- alignExtent(j, h)
k <- alignExtent(k, i)

tz.bb <- stack(tz.bb, j)
tz.lb <- stack(tz.lb, k)

tz.ya <- stack(list.files(path = "~/Google Drive/Data/MICORDEA/Results/GPS3 Final Yields tiff", pattern = "[[:alnum:]]+_att_[[:graph:]]+.tif$", full.names = TRUE))
#### End data import ####

#### Begin data visualisation ####
hist(tz.bb)
hist(tz.lb)
hist(tz.ya)

plot(tz.bb)
plot(tz.lb)
plot(tz.ya)

#### End data visualisation ####


#eos

yl_50b1_lb1 <- raster("~/Google Drive/Data/MICORDEA/Results/GPS3 Final Yields tiff/yl_b150_lb1.tif")
yl_30b1_lb1 <- raster("~/Google Drive/Data/MICORDEA/Results/GPS3 Final Yields tiff/yl_b130_lb1.tif")
yl_30a2_lb1 <- raster("~/Google Drive/Data/MICORDEA/Results/GPS3 Final Yields tiff/yl_a230_lb1.tif")

