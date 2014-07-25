##############################################################################
# title         : MICORDEA_TZ_AUDPC.R;
# purpose       : extract and generate tables of AUDPC from EPIRICE;
#               : for publication;
# producer      : prepared by A. Sparks;
# last update   : in Los Baños, Philippines, Jul. 2014;
# inputs        : GTiff files of yield losses for Tanzania calculated using RICEPEST;
# outputs       : AUDPC value data to be used in publication table;
# remarks 1     : ;
# Licence:      : GPL2;
##############################################################################

#### Begin libraries ####
library(raster)
library(ggplot2)
library(beepr)
library(grid)
library(scales)
library(extrafont)
##### End libraries ####

#### Begin data import ####
## Leaf blast percent lesion coverage files ##
tz.base.lb <- stack(list.files(path = "~/Google Drive/Data/MICORDEA/Disease Modelling/base/", pattern = "[[:graph:]]+blast_audpc.tif$", full.names = TRUE)) # stack all files for blast, base time slice/scenario
tz.base.lb[tz.base.lb == -9999] <- NA # set -9999 values to NA for R

tz.2030.a2.lb <- stack(list.files(path = "~/Google Drive/Data/MICORDEA/Disease Modelling/2030/a2/", pattern = "[[:graph:]]+blast_audpc.tif$", full.names = TRUE)) # A2 2030
tz.2030.a2.lb[tz.2030.a2.lb == -9999] <- NA
tz.2050.a2.lb <- stack(list.files(path = "~/Google Drive/Data/MICORDEA/Disease Modelling/2050/a2/", pattern = "[[:graph:]]+blast_audpc.tif$", full.names = TRUE)) # A2 2050
tz.2050.a2.lb[tz.2050.a2.lb == -9999] <- NA

tz.2030.ab.lb <- stack(list.files(path = "~/Google Drive/Data/MICORDEA/Disease Modelling/2030/ab/", pattern = "[[:graph:]]+blast_audpc.tif$", full.names = TRUE)) # A1B 2030
tz.2030.ab.lb[tz.2030.ab.lb == -9999] <- NA
tz.2050.ab.lb <- stack(list.files(path = "~/Google Drive/Data/MICORDEA/Disease Modelling/2050/ab/", pattern = "[[:graph:]]+blast_audpc.tif$", full.names = TRUE)) # A1B 2050
tz.2050.ab.lb[tz.2050.ab.lb == -9999] <- NA

tz.2030.b1.lb <- stack(list.files(path = "~/Google Drive/Data/MICORDEA/Disease Modelling/2030/b1/", pattern = "[[:graph:]]+blast_audpc.tif$", full.names = TRUE)) # B1 2030
tz.2030.b1.lb[tz.2030.b1.lb == -9999] <- NA
tz.2050.b1.lb <- stack(list.files(path = "~/Google Drive/Data/MICORDEA/Disease Modelling/2050/b1/", pattern = "[[:graph:]]+blast_audpc.tif$", full.names = TRUE)) # B1 2050
tz.2050.b1.lb[tz.2050.b1.lb == -9999] <- NA

## Bacterial blight percent lesion coverage files ##
tz.base.bb <- stack(list.files(path = "~/Google Drive/Data/MICORDEA/Disease Modelling/base/", pattern = "[[:graph:]]+bblight_audpc.tif$", full.names = TRUE)) # Base
tz.base.bb[tz.base.bb == -9999] <- NA

tz.2030.a2.bb <- stack(list.files(path = "~/Google Drive/Data/MICORDEA/Disease Modelling/2030/a2/", pattern = "[[:graph:]]+bblight_audpc.tif$", full.names = TRUE)) # A2 2030
tz.2030.a2.bb[tz.2030.a2.bb == -9999] <- NA
tz.2050.a2.bb <- stack(list.files(path = "~/Google Drive/Data/MICORDEA/Disease Modelling/2050/a2/", pattern = "[[:graph:]]+bblight_audpc.tif$", full.names = TRUE)) # A2 2050
tz.2050.a2.bb[tz.2050.a2.bb == -9999] <- NA

tz.2030.ab.bb <- stack(list.files(path = "~/Google Drive/Data/MICORDEA/Disease Modelling/2030/ab/", pattern = "[[:graph:]]+bblight_audpc.tif$", full.names = TRUE)) # A1B 2030
tz.2030.ab.bb[tz.2030.ab.bb == -9999] <- NA
tz.2050.ab.bb <- stack(list.files(path = "~/Google Drive/Data/MICORDEA/Disease Modelling/2050/ab/", pattern = "[[:graph:]]+bblight_audpc.tif$", full.names = TRUE)) # A1B 2050
tz.2050.ab.bb[tz.2050.ab.bb == -9999] <- NA

tz.2030.b1.bb <- stack(list.files(path = "~/Google Drive/Data/MICORDEA/Disease Modelling/2030/b1/", pattern = "[[:graph:]]+bblight_audpc.tif$", full.names = TRUE)) # B1 2030
tz.2030.b1.bb[tz.2030.b1.bb == -9999] <- NA
tz.2050.b1.bb <- stack(list.files(path = "~/Google Drive/Data/MICORDEA/Disease Modelling/2050/b1/", pattern = "[[:graph:]]+bblight_audpc.tif$", full.names = TRUE)) # B1 2050
tz.2050.b1.bb[tz.2050.b1.bb == -9999] <- NA
#### End data import ####

#### Extract data for tabular purposes ####
lb <- data.frame(round(values(tz.base.lb), 0), round(values(tz.2030.ab.lb), 0), round(values(tz.2050.ab.lb), 0), round(values(tz.2030.a2.lb), 0), round(values(tz.2050.a2.lb), 0), round(values(tz.2030.b1.lb), 0), round(values(tz.2050.b1.lb), 0))
bb <- data.frame(round(values(tz.base.bb), 0), round(values(tz.2030.ab.bb), 0), round(values(tz.2050.ab.bb), 0), round(values(tz.2030.a2.bb), 0), round(values(tz.2050.a2.bb), 0), round(values(tz.2030.b1.bb), 0), round(values(tz.2050.b1.bb), 0))

## Mean and SD for tabular representation ##
lb.table <- data.frame(round(apply(lb, 2, mean, na.rm = TRUE), 0), 
                       round(apply(lb, 2, sd, na.rm = TRUE), 0)) # create matrix of average/sd leaf blast AUDPC
bb.table <- data.frame(round(apply(bb, 2, mean, na.rm = TRUE), 0), 
                       round(apply(bb, 2, sd, na.rm = TRUE), 0)) # create matrix of average/sd bacterial leaf blight AUDPC

## Names in table ##
row.names(lb.table) <- row.names(bb.table) <- 1:length(lb.table[, 1])
names(lb.table) <- c("LB.Mean.AUDPC", "LB.SD.AUDPC")
names(bb.table) <- c("BB.Mean.AUDPC", "BB.SD.AUDPC")


#### Extract data and make dataframe for visualisation ####
lb <- c(round(values(tz.base.lb), 0), round(values(tz.2030.a1b.lb), 0), round(values(tz.2050.a1b.lb), 0), round(values(tz.2030.a2.lb), 0), round(values(tz.2050.a2.lb), 0), round(values(tz.2030.b1.lb), 0), round(values(tz.2050.b1.lb), 0))
bb <- c(round(values(tz.base.bb), 0), round(values(tz.2030.a1b.bb), 0), round(values(tz.2050.a1b.bb), 0), round(values(tz.2030.a2.bb), 0), round(values(tz.2050.a2.bb), 0), round(values(tz.2030.b1.bb), 0), round(values(tz.2050.b1.bb), 0))

x <- c(rep("Base", ncell(tz.2050.b1.bb)), rep("A1b", ncell(tz.2050.b1.bb)*2), rep("A2", ncell(tz.2050.b1.bb)*2), rep("B1", ncell(tz.2050.b1.bb)*2)) # Create vector of emission scenario
y <- c(rep(2000, ncell(tz.2050.b1.bb)), rep(c(2030, 2050), each = ncell(tz.2050.b1.bb), times = 3)) # Create vector of time-slice midpoint

lb <- data.frame(x, y, lb) # Combind the vectors into one dataframe for ggplot2
bb <- data.frame(x, y, bb)

names(lb) <- c("Scenario", "Time.Slice", "Leaf.Blast.AUDPC") # Assign names to leaf blast data frame
names(bb) <- c("Scenario", "Time.Slice", "Bacterial.Blight.AUDPC") # Assign names to bacterial blight data frame

lb[, 1] <- factor(lb[, 1], levels = c("Base", "A1B", "A2", "B1")) # Fix the order of the facets in the plot, put the base first, then A1B the A2...
bb[, 1] <- factor(bb[, 1], levels = c("Base", "A1B", "A2", "B1"))

#### End extract data and make dataframe for visualisation ####

p <- ggplot(data = lb, aes(x = Time.Slice, y = Leaf.Blast.AUDPC, group = Time.Slice)) +
  geom_boxplot(width = 10, outlier.shape = NA) +
  scale_x_continuous("Time Slice") + scale_y_continuous("Leaf Blast AUDPC Values", limits = c(0, max(na.omit(bb[, 3])))) +
  theme(axis.title.x = element_text(size = 10, family = "Helvetica"),
        axis.title.y = element_text(size = 10, angle = 90, family = "Helvetica"),
        legend.background = element_rect(fill = "white", colour = "black"),
        legend.key.width = unit(11, "mm"),
        legend.text = element_text(size = 8, family = "Helvetica"),
        panel.grid.minor = element_blank(), # switch off minor gridlines
        panel.border = element_rect(colour = "black", unit(0.25, "mm")),
        legend.title = element_blank(),
        legend.key.size = unit(1, "lines"),
        legend.key = element_blank(),
        legend.key.size = unit(1, "lines"),
        axis.ticks.length = unit(0.15 , "cm"))
p + facet_grid(. ~ Scenario)

ggsave("../Latex/Figures/LB_AUDPC.eps", width = 140, height = 120, units = "mm")

p <- ggplot(data = bb, aes(x = Time.Slice, y = Bacterial.Blight.AUDPC, group = Time.Slice)) +
  geom_boxplot(width = 10, outlier.shape = NA) +
  scale_x_continuous("Time Slice") + scale_y_continuous("Bacterial Blight AUDPC Values") +
  theme(axis.title.x = element_text(size = 10, family = "Helvetica"),
        axis.title.y = element_text(size = 10, angle = 90, family = "Helvetica"),
        legend.position = c(0.1, 0.85), 
        legend.background = element_rect(fill = "white", colour = "black"),
        legend.key.width = unit(11, "mm"),
        legend.text = element_text(size = 8, family = "Helvetica"),
        panel.grid.minor = element_blank(), # switch off minor gridlines
        panel.border = element_rect(colour = "black", unit(0.25, "mm")),
        legend.title = element_blank(),
        legend.key.size = unit(1, "lines"),
        legend.key = element_blank(),
        legend.key.size = unit(1, "lines"),
        axis.ticks.length = unit(0.15 , "cm"))
p + facet_grid(. ~ Scenario)

ggsave("../Latex/Figures/BB_AUDPC.eps", width = 140, height = 120, units = "mm")

#eos
