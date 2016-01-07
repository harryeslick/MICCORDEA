################################################################################
# title         : Install_Libraries.R;
# purpose       : install libraries necessary for analysis in R;
# producer      : prepared by A. Sparks;
# last update   : in Los Baños, Philippines, May 2015;
# inputs        : na;
# outputs       : na;
# remarks 1     : ;
# Licence:      : GPL2;
################################################################################

install.packages(c("ggplot2",
                   "raster",
                   "grid",
                   "scales",
                   "extrafont",
                   "wesanderson",
                   "ggthemes",
                   "plotKML"),
                 dep = TRUE)

library(extrafont)
font_import()
loadfonts()

# eos

