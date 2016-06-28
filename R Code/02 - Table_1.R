################################################################################
# title         : Table_1.R;
# purpose       : extract temperature values for Table 1;
# producer      : prepared by A. Sparks;
# last update   : Toowoomba, Qld, Jun 2016;
# inputs        : Generated weather files from C. Duku;
# outputs       : Tables of temperatures at different time slices;
# remarks 1     : ;
# Licence:      : GPL2;
################################################################################

# Libraries --------------------------------------------------------------------
library(raster)
library(ggplot2)
library(grid)
library(reshape2)
library(dplyr)

source("Functions/Get_Data.R")

# Load data --------------------------------------------------------------------

download_data() # get data from Figshare

TZ <- getData("GADM", country = "TZA", level = 0, path = "../Data")

tmp_base <- stack(list.files(path = "../Data/base",
                             pattern = "tmean[[:graph:]]{6}$",
                             full.names = TRUE))
tmp_a230 <- stack(list.files(path = "../Data/a230",
                             pattern = "tmean[[:graph:]]{6}$",
                             full.names = TRUE))
tmp_a250 <- stack(list.files(path = "../Data/a250",
                             pattern = "tmean[[:graph:]]{6}$",
                             full.names = TRUE))
tmp_b130 <- stack(list.files(path = "../Data/b130",
                             pattern = "tmean[[:graph:]]{6}$",
                             full.names = TRUE))
tmp_b150 <- stack(list.files(path = "../Data/b150",
                             pattern = "tmean[[:graph:]]{6}$",
                             full.names = TRUE))
tmp_ab30 <- stack(list.files(path = "../Data/ab30",
                             pattern = "tmean[[:graph:]]{6}$",
                             full.names = TRUE))
tmp_ab50 <- stack(list.files(path = "../Data/ab50",
                             pattern = "tmean[[:graph:]]{6}$",
                             full.names = TRUE))


# Data munging----- ------------------------------------------------------------
tmp_base_avg <- mean(cellStats(tmp_base, stat = "mean"))
tmp_a230_avg <- mean(cellStats(tmp_a230, stat = "mean"))
tmp_a250_avg <- mean(cellStats(tmp_a250, stat = "mean"))
tmp_ab30_avg <- mean(cellStats(tmp_ab30, stat = "mean"))
tmp_ab50_avg <- mean(cellStats(tmp_ab50, stat = "mean"))
tmp_b130_avg <- mean(cellStats(tmp_b130, stat = "mean"))
tmp_b150_avg <- mean(cellStats(tmp_b150, stat = "mean"))

table_1 <- data.frame(round(tmp_base_avg, 2),
                            round(mean(cellStats(tmp_a230, stat = "mean")), 2),
                            round(mean(cellStats(tmp_a250, stat = "mean")), 2),
                            round(mean(cellStats(tmp_ab30, stat = "mean")), 2),
                            round(mean(cellStats(tmp_ab50, stat = "mean")), 2),
                            round(mean(cellStats(tmp_b130, stat = "mean")), 2),
                            round(mean(cellStats(tmp_b150, stat = "mean")), 2),
                            round(tmp_a230_avg-tmp_base_avg, 2),
                            round(tmp_a250_avg-tmp_base_avg, 2),
                            round(tmp_ab30_avg-tmp_base_avg, 2),
                            round(tmp_ab50_avg-tmp_base_avg, 2),
                            round(tmp_b130_avg-tmp_base_avg, 2),
                            round(tmp_b150_avg-tmp_base_avg, 2))

names(table_1) <- c("Base",
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

# Display final values for table 1 in final publication ------------------------
table_1
# eos
