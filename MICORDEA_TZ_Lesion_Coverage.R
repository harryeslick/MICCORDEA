##############################################################################
# title         : MICORDEA_TZ_Lesion_Coverage.R;
# purpose       : analyse the lesion coverage results from the MICORDEA project
#               : for publication;
# producer      : prepared by A. Sparks;
# last update   : in Los Baños, Philippines, Jun. 2014;
# inputs        : GTiff files of yield losses for Tanzania calculated using RICEPEST;
# outputs       : Histograms and maps of lesion coverages for base/2030/2050 a2/b1/ab scenario;
# remarks 1     : ;
# Licence:      : GPL2;
##############################################################################

#### Begin libraries ####
library(raster)
##### End libraries ####

#### Begin data import ####
## Leaf blast files ##
tz.base.lb <- stack(list.files(path = "~/Google Drive/Data/MICORDEA/Disease Modelling/base/", pattern = "[[:graph:]]+blast_[[:digit:]]{2,3}.tif$", full.names = TRUE))
tz.2030.a2.lb <- stack(list.files(path = "~/Google Drive/Data/MICORDEA/Disease Modelling/2030/a2/", pattern = "[[:graph:]]+blast_[[:digit:]]{2,3}.tif$", full.names = TRUE))
tz.2050.a2.lb <- stack(list.files(path = "~/Google Drive/Data/MICORDEA/Disease Modelling/2050/a2/", pattern = "[[:graph:]]+blast_[[:digit:]]{2,3}.tif$", full.names = TRUE))

tz.2030.ab.lb <- stack(list.files(path = "~/Google Drive/Data/MICORDEA/Disease Modelling/2030/ab/", pattern = "[[:graph:]]+blast_[[:digit:]]{2,3}.tif$", full.names = TRUE))
tz.2050.ab.lb <- stack(list.files(path = "~/Google Drive/Data/MICORDEA/Disease Modelling/2050/ab/", pattern = "[[:graph:]]+blast_[[:digit:]]{2,3}.tif$", full.names = TRUE))

tz.2030.b1.lb <- stack(list.files(path = "~/Google Drive/Data/MICORDEA/Disease Modelling/2030/b1/", pattern = "[[:graph:]]+blast_[[:digit:]]{2,3}.tif$", full.names = TRUE))
tz.2050.b1.lb <- stack(list.files(path = "~/Google Drive/Data/MICORDEA/Disease Modelling/2050/b1/", pattern = "[[:graph:]]+blast_[[:digit:]]{2,3}.tif$", full.names = TRUE))

## Bacterial Blight Files ##
tz.base.bb <- stack(list.files(path = "~/Google Drive/Data/MICORDEA/Disease Modelling/base/", pattern = "[[:graph:]]+bblight_[[:digit:]]{2,3}.tif$", full.names = TRUE))
tz.2030.a2.bb <- stack(list.files(path = "~/Google Drive/Data/MICORDEA/Disease Modelling/2030/a2/", pattern = "[[:graph:]]+bblight_[[:digit:]]{2,3}.tif$", full.names = TRUE))
tz.2050.a2.bb <- stack(list.files(path = "~/Google Drive/Data/MICORDEA/Disease Modelling/2050/a2/", pattern = "[[:graph:]]+bblight_[[:digit:]]{2,3}.tif$", full.names = TRUE))

tz.2030.ab.bb <- stack(list.files(path = "~/Google Drive/Data/MICORDEA/Disease Modelling/2030/ab/", pattern = "[[:graph:]]+bblight_[[:digit:]]{2,3}.tif$", full.names = TRUE))
tz.2050.ab.bb <- stack(list.files(path = "~/Google Drive/Data/MICORDEA/Disease Modelling/2050/ab/", pattern = "[[:graph:]]+bblight_[[:digit:]]{2,3}.tif$", full.names = TRUE))

tz.2030.b1.bb <- stack(list.files(path = "~/Google Drive/Data/MICORDEA/Disease Modelling/2030/b1/", pattern = "[[:graph:]]+bblight_[[:digit:]]{2,3}.tif$", full.names = TRUE))
tz.2050.b1.bb <- stack(list.files(path = "~/Google Drive/Data/MICORDEA/Disease Modelling/2050/b1/", pattern = "[[:graph:]]+bblight_[[:digit:]]{2,3}.tif$", full.names = TRUE))
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
