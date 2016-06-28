################################################################################
# title         : Figures_4_5_6_and_7.R;
# purpose       : generate figures 4, 5, 6 and 7 for publication;
# producer      : prepared by A. Sparks;
# last update   : in Toowoomba, Qld. Jun 2016;
# inputs        : ESRI files of yield losses and attainable yield for Tanzania
#               : calculated using RICEPEST;
# outputs       : Histograms, maps and tabular data of yield losses for
#               : base/2030/2050 a2/b1/ab scenarios;
# remarks 1     : ;
# Licence:      : GPL2;
################################################################################

# Libraries --------------------------------------------------------------------
library(raster)
library(ggplot2)
library(reshape2)
library(extrafont)
library(dplyr)
library(wesanderson)
library(ggthemes)
library(scales)
library(Hmisc)

loadfonts(device = "postscript")

# Download data ----------------------------------------------------------------
source("Functions/Get_Data.R")
download_data() # download output from Figshare

# Load data-- ------------------------------------------------------------------

TZ <- getData("GADM", country = "TZA", level = 0, path = "../Data")

tz_bb <- stack(list.files(path = "../Data/RICEPEST Modified GPS3 Output",
                          pattern = "^[a,b].*bb$", full.names = TRUE))
crs(tz_bb) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
tz_lb <- stack(list.files(path = "../Data/RICEPEST Modified GPS3 Output",
                          pattern = "^[a,b].*lb$", full.names = TRUE))
crs(tz_lb) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
tz_ya <- stack(list.files(path = "../Data/RICEPEST Modified GPS3 Output",
                          pattern = "^[a,b].*att$", full.names = TRUE))
crs(tz_ya) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"


# Data munging -----------------------------------------------------------------

# Calculate the yield losses
tz_bb.loss <- (tz_ya-tz_bb)
tz_lb.loss <- (tz_ya-tz_lb)

# BB
p_bb_loss_a230 <- data.frame(rasterToPoints(tz_bb.loss[[1]]))
p_bb_loss_a250 <- data.frame(rasterToPoints(tz_bb.loss[[2]]))
p_bb_loss_ab30 <- data.frame(rasterToPoints(tz_bb.loss[[3]]))
p_bb_loss_ab50 <- data.frame(rasterToPoints(tz_bb.loss[[4]]))
p_bb_loss.b130 <- data.frame(rasterToPoints(tz_bb.loss[[5]]))
p_bb_loss.b150 <- data.frame(rasterToPoints(tz_bb.loss[[6]]))
p_bb_loss.base <- data.frame(rasterToPoints(tz_bb.loss[[7]]))

# Join data frames (future and base)
p_bb_loss_a230 <- inner_join(p_bb_loss_a230, p_bb_loss.base)
p_bb_loss_a250 <- inner_join(p_bb_loss_a250, p_bb_loss.base)
p_bb_loss_ab30 <- inner_join(p_bb_loss_ab30, p_bb_loss.base)
p_bb_loss_ab50 <- inner_join(p_bb_loss_ab50, p_bb_loss.base)
p_bb_loss.b130 <- inner_join(p_bb_loss.b130, p_bb_loss.base)
p_bb_loss.b150 <- inner_join(p_bb_loss.b150, p_bb_loss.base)

# Calculate change in loss
p_bb_loss_a230 <- mutate(p_bb_loss_a230, Change = a230_att-base_att)
p_bb_loss_a250 <- mutate(p_bb_loss_a250, Change = a250_att-base_att)
p_bb_loss_ab30 <- mutate(p_bb_loss_ab30, Change = ab30_att-base_att)
p_bb_loss_ab50 <- mutate(p_bb_loss_ab50, Change = ab50_att-base_att)
p_bb_loss.b130 <- mutate(p_bb_loss.b130, Change = b130_att-base_att)
p_bb_loss.b150 <- mutate(p_bb_loss.b150, Change = b150_att-base_att)

# LB
# Create data frames from rasters
p_lb_loss_a230 <- data.frame(rasterToPoints(tz_lb.loss[[1]]))
p_lb_loss_a250 <- data.frame(rasterToPoints(tz_lb.loss[[2]]))
p_lb_loss_ab30 <- data.frame(rasterToPoints(tz_lb.loss[[3]]))
p_lb_loss_ab50 <- data.frame(rasterToPoints(tz_lb.loss[[4]]))
p_lb_loss.b130 <- data.frame(rasterToPoints(tz_lb.loss[[5]]))
p_lb_loss.b150 <- data.frame(rasterToPoints(tz_lb.loss[[6]]))
p_lb_loss.base <- data.frame(rasterToPoints(tz_lb.loss[[7]]))

# Join data frames (future and base)
p_lb_loss_a230 <- inner_join(p_lb_loss_a230, p_lb_loss.base)
p_lb_loss_a250 <- inner_join(p_lb_loss_a250, p_lb_loss.base)
p_lb_loss_ab30 <- inner_join(p_lb_loss_ab30, p_lb_loss.base)
p_lb_loss_ab50 <- inner_join(p_lb_loss_ab50, p_lb_loss.base)
p_lb_loss.b130 <- inner_join(p_lb_loss.b130, p_lb_loss.base)
p_lb_loss.b150 <- inner_join(p_lb_loss.b150, p_lb_loss.base)

# Calculate change in loss
p_lb_loss_a230 <- mutate(p_lb_loss_a230, Change = a230_att-base_att)
p_lb_loss_a250 <- mutate(p_lb_loss_a250, Change = a250_att-base_att)
p_lb_loss_ab30 <- mutate(p_lb_loss_ab30, Change = ab30_att-base_att)
p_lb_loss_ab50 <- mutate(p_lb_loss_ab50, Change = ab50_att-base_att)
p_lb_loss.b130 <- mutate(p_lb_loss.b130, Change = b130_att-base_att)
p_lb_loss.b150 <- mutate(p_lb_loss.b150, Change = b150_att-base_att)


# Make appropriate column names
names(p_bb_loss_a230) <-
  names(p_bb_loss_a250) <-
  names(p_bb_loss_ab30) <-
  names(p_bb_loss_ab50) <-
  names(p_bb_loss.b130) <-
  names(p_bb_loss.b150) <- c("Longitude", "Latitude", "Future", "Base", "MAP")
names(p_lb_loss_a230) <-
  names(p_lb_loss_a250) <-
  names(p_lb_loss_ab30) <-
  names(p_lb_loss_ab50) <-
  names(p_lb_loss.b130) <-
  names(p_lb_loss.b150) <- c("Longitude", "Latitude", "Future", "Base", "MAP")

# Create data frames for violin plots
ya <- na.omit(unlist(values(tz_ya)))
ya <- ya[, c(7, 1, 2, 3, 4, 5, 6)]

bb <- na.omit(unlist(values(tz_bb.loss)))
bb <- bb[, c(7, 1, 2, 3, 4, 5, 6)]

lb <- na.omit(unlist(values(tz_lb.loss)))
lb <- lb[, c(7, 1, 2, 3, 4, 5, 6)]

x <- c(rep("Base 2000", length(bb[, 1])),
       rep("A2 2030", length(bb[, 1])),
       rep("A2 2050", length(bb[, 1])),
       rep("A1B 2030", length(bb[, 1])),
       rep("A1B 2050", length(bb[, 1])),
       rep("B1 2030", length(bb[, 1])),
       rep("B1 2050", length(bb[, 1])))
scenarios <- c(rep("Base", length(bb[, 1])),
               rep("A2", length(bb[, 1])*2),
               rep("A1B", length(bb[, 1])*2),
               rep("B1", length(bb[, 1])*2))

ya <- as.vector(ya)
ya <- data.frame(x, scenarios, ya)
ya[, 1] <- factor(ya[, 1], levels = unique(as.character(ya[, 1])))

bb <- as.vector(bb)
bb <- data.frame(x, scenarios, bb)
bb[, 1] <- factor(bb[, 1], levels = unique(as.character(bb[, 1])))

lb <- as.vector(lb)
lb <- data.frame(x, scenarios, lb)
lb[, 1] <- factor(lb[, 1], levels = unique(as.character(lb[, 1])))


# Cut data for mapping
p_bb_loss_a230$GROUP <- as.numeric(cut(p_bb_loss_a230$MAP,
                                       include.lowest = TRUE,
                                       breaks = seq(-0.79, 0.56, by = 0.25)))
p_bb_loss_a250$GROUP <- as.numeric(cut(p_bb_loss_a250$MAP,
                                       include.lowest = TRUE,
                                       breaks = seq(-0.79, 0.56, by = 0.25)))
p_bb_loss_ab30$GROUP <- as.numeric(cut(p_bb_loss_ab30$MAP,
                                       include.lowest = TRUE,
                                       breaks = seq(-0.79, 0.56, by = 0.25)))
p_bb_loss_ab50$GROUP <- as.numeric(cut(p_bb_loss_ab50$MAP,
                                       include.lowest = TRUE,
                                       breaks = seq(-0.79, 0.56, by = 0.25)))
p_bb_loss.b130$GROUP <- as.numeric(cut(p_bb_loss.b130$MAP,
                                       include.lowest = TRUE,
                                       breaks = seq(-0.79, 0.56, by = 0.25)))
p_bb_loss.b150$GROUP <- as.numeric(cut(p_bb_loss.b150$MAP,
                                       include.lowest = TRUE,
                                       breaks = seq(-0.79, 0.56, by = 0.25)))

p_bb_loss <- rbind(p_bb_loss_a230,
                   p_bb_loss_a250,
                   p_bb_loss_ab30,
                   p_bb_loss_ab50,
                   p_bb_loss.b130,
                   p_bb_loss.b150)
SCENARIO <- c(rep("A2", length(p_bb_loss_a230[, 1])),
              rep("A2", length(p_bb_loss_ab30[, 1])),
              rep("AB", length(p_bb_loss_ab30[, 1])),
              rep("AB", length(p_bb_loss_ab50[, 1])),
              rep("B1", length(p_bb_loss.b130[, 1])),
              rep("B1", length(p_bb_loss.b150[, 1])))
TIMESLICE <- c(rep(2030, length(p_bb_loss_a230[, 1])),
               rep(2050, length(p_bb_loss_ab30[, 1])),
               rep(2030, length(p_bb_loss_ab30[, 1])),
               rep(2050, length(p_bb_loss_ab50[, 1])),
               rep(2030, length(p_bb_loss.b130[, 1])),
               rep(2050, length(p_bb_loss.b150[, 1])))

p_bb_loss <- cbind(p_bb_loss, SCENARIO, TIMESLICE)
p_bb_loss$GROUP <- as.factor(p_bb_loss$GROUP)


# Begin data visualisation -----------------------------------------------------

# Violin plots of yield loss by time slice and scenario
# Attainable yield, Figure 4
figure_4 <- ggplot(ya, aes(x = x, y = ya))
figure_4 <- figure_4 + geom_violin(aes(colour = as.factor(scenarios),
                                       fill = as.factor(scenarios))) +
  scale_color_manual(values = wes_palette("Darjeeling2")) +
  scale_fill_manual(values = wes_palette("Darjeeling2")) +
  labs(x = "Scenario and Time Slice", y = "Yield (tons/ha)") +
  theme_few() +
  theme(legend.position = "none") +
  theme(axis.title.x = element_text(size = 10, family = "Helvetica"),
        axis.title.y = element_text(size = 10, angle = 90, family = "Helvetica"),
        axis.text = element_text(size = 9, family = "Helvetica"))
ggsave(filename = "Fig4.eps", path = "../LaTeX/Figures/", width = 140,
       height = 140, units = "mm")

# LB, Figure 5
figure_5 <- ggplot(lb, aes(x = x, y = lb))
figure_5 <- figure_5 +
  geom_violin(aes(colour = as.factor(scenarios), fill = as.factor(scenarios))) +
  scale_color_manual(values = wes_palette("Darjeeling2")) +
  scale_fill_manual(values = wes_palette("Darjeeling2")) +
  labs(x = "Scenario and Time Slice", y = "Yield loss (tons/ha)") +
  theme_few() +
  theme(legend.position = "none") +
  theme(axis.title.x = element_text(size = 10, family = "Helvetica"),
        axis.title.y = element_text(size = 10, angle = 90,
                                    family = "Helvetica"),
        axis.text = element_text(size = 9, family = "Helvetica"),
        plot.margin = unit(c(.5, .5, .5, .5), "lines"))
ggsave(filename = "Fig5.eps", path = "../LaTeX/Figures/", width = 140,
       height = 140, units = "mm")

# BB, Figure 6
figure_6 <- ggplot(bb, aes(x = x, y = bb))
figure_6 <- figure_6 +
  geom_violin(aes(colour = as.factor(scenarios), fill = as.factor(scenarios))) +
  scale_color_manual(values = wes_palette("Darjeeling2")) +
  scale_fill_manual(values = wes_palette("Darjeeling2")) +
  labs(x = "Scenario and Time Slice", y = "Yield loss (tons/ha)") +
  theme_few() +
  theme(legend.position = "none") +
  theme(axis.title.x = element_text(size = 10, family = "Helvetica"),
        axis.title.y = element_text(size = 10, angle = 90, family = "Helvetica"),
        axis.text = element_text(size = 9, family = "Helvetica"),
        plot.margin = unit(c(.5, .5, .5, .5), "lines"))
ggsave(filename = "Fig6.eps", path = "../LaTeX/Figures/", width = 140,
       height = 140, units = "mm")

# Map of yield loss for BB
figure_7 <- ggplot(data = p_bb_loss, aes(y = Latitude, x = Longitude,
                                         fill = GROUP, colour = GROUP)) +
  geom_polygon(data = TZ, aes(x = long, y = lat, group = group),
               colour = "#333333", fill = "#333333") +
  geom_tile(size = 0.4) + # eliminates lines between the cell
  scale_colour_brewer(type = "div",
                      palette = "RdYlBu",
                      labels = c("-0.79, -0.54", "-0.54, -0.29", "-0.29, 0.04",
                                 " 0.40, 0.21", " 0.21, 0.46"),
                      expression(paste("t ", ha^"-1"))) +

  scale_fill_brewer(type = "div",
                    palette = "RdYlBu",
                    labels = c("-0.79, -0.54", "-0.54, -0.29", "-0.29, 0.04",
                               " 0.04, 0.21", " 0.21, 0.46"),
                    expression(paste("t ", ha ^"-1"))) +
  theme_few() +
  theme(axis.title.x = element_text(size = 10, family = "Helvetica"),
        axis.title.y = element_text(size = 10, angle = 90,
                                    family = "Helvetica"),
        axis.text = element_text(size = 9, family = "Helvetica"),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "lines")) +
  coord_equal() +
  facet_grid(TIMESLICE ~ SCENARIO) +
  coord_map("cylindrical") # use cylindrical projection at low latitude

ggsave("Fig7.eps", path = "../Latex/figures", width = 191, height = 116,
       units = "mm")

# Table data -------------------------------------------------------------------

round(summary(tz_ya), 4)
round(summary(tz_bb.loss), 4)
round(summary(tz_lb.loss), 4)

cellStats(tz_ya, stat = "mean", na.rm = TRUE)
cellStats(tz_bb.loss, stat = "mean", na.rm = TRUE)
cellStats(tz_lb.loss, stat = "mean", na.rm = TRUE)

# eos
