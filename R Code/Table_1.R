##############################################################################
# title         : Table_1.R;
# purpose       : extract temperature values for Table 1;
# producer      : prepared by A. Sparks;
# last update   : in Los Baños, Philippines, May 2015;
# inputs        : Generated weather files from C. Duku;
# outputs       : Tables of temperatures at different time slices;
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
tmp.base <- stack(list.files(path = "~/tmp/RICEPEST Data/base", 
                             pattern = "tmean[[:graph:]]{6}$", full.names = TRUE))
tmp.a230 <- stack(list.files(path = "~/tmp/RICEPEST Data/a230", 
                             pattern = "tmean[[:graph:]]{6}$", full.names = TRUE))
tmp.a250 <- stack(list.files(path = "~/tmp/RICEPEST Data/a250", 
                             pattern = "tmean[[:graph:]]{6}$", full.names = TRUE))
tmp.b130 <- stack(list.files(path = "~/tmp/RICEPEST Data/b130", 
                             pattern = "tmean[[:graph:]]{6}$", full.names = TRUE))
tmp.b150 <- stack(list.files(path = "~/tmp/RICEPEST Data/b150", 
                             pattern = "tmean[[:graph:]]{6}$", full.names = TRUE))
tmp.ab30 <- stack(list.files(path = "~/tmp/RICEPEST Data/ab30",
                             pattern = "tmean[[:graph:]]{6}$", full.names = TRUE))
tmp.ab50 <- stack(list.files(path = "~/tmp/RICEPEST Data/ab50", 
                             pattern = "tmean[[:graph:]]{6}$", full.names = TRUE))

#### End data import ####

#### Data manipulation ####
tmp.base.avg <- mean(cellStats(tmp.base, stat = "mean"))
tmp.a230.avg <- mean(cellStats(tmp.a230, stat = "mean"))
tmp.a250.avg <- mean(cellStats(tmp.a250, stat = "mean"))
tmp.ab30.avg <- mean(cellStats(tmp.ab30, stat = "mean"))
tmp.ab50.avg <- mean(cellStats(tmp.ab50, stat = "mean"))
tmp.b130.avg <- mean(cellStats(tmp.b130, stat = "mean"))
tmp.b150.avg <- mean(cellStats(tmp.b150, stat = "mean"))

table.1 <- data.frame(round(tmp.base.avg, 2),
                            round(mean(cellStats(tmp.a230, stat = "mean")), 2),
                            round(mean(cellStats(tmp.a250, stat = "mean")), 2),
                            round(mean(cellStats(tmp.ab30, stat = "mean")), 2),
                            round(mean(cellStats(tmp.ab50, stat = "mean")), 2),
                            round(mean(cellStats(tmp.b130, stat = "mean")), 2),
                            round(mean(cellStats(tmp.b150, stat = "mean")), 2),
                            round(tmp.a230.avg-tmp.base.avg, 2),
                            round(tmp.a250.avg-tmp.base.avg, 2),
                            round(tmp.ab30.avg-tmp.base.avg, 2),
                            round(tmp.ab50.avg-tmp.base.avg, 2),
                            round(tmp.b130.avg-tmp.base.avg, 2),
                            round(tmp.b150.avg-tmp.base.avg, 2))

names(table.1) <- c("Base", 
                        "a230", 
                        "a250", 
                        "ab30", 
                        "ab50", 
                        "b130", 
                        "b150", 
                        "a230 Increase", 
                        "a250 Increase", 
                        "ab30 Increase", 
                        "ab50 Increase", 
                        "b130 Increase", 
                        "b150 Increase")

table.1 # view results

#eos
