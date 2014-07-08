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
tz.bb <- stack(list.files(path = "~/Google Drive/Data/MICORDEA/Results/GPS3 Final Yields tiff", pattern = "[[:graph:]]_bb1.tif$", full.names = TRUE))
tz.lb <- stack(list.files(path = "~/Google Drive/Data/MICORDEA/Results/GPS3 Final Yields tiff", pattern = "[[:graph:]]_lb1.tif$", full.names = TRUE))
tz.ya <- stack(list.files(path = "~/Google Drive/Data/MICORDEA/Results/GPS3 Final Yields tiff", pattern = "[[:graph:]]_att1.tif$", full.names = TRUE))

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
