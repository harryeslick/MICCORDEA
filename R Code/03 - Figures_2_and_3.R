################################################################################
# title         : Figures_2_and_3.R;
# purpose       : generate figures 2 and 3 for publication;
# producer      : prepared by A. Sparks;
# last update   : in Toowoomba, Qld. Jun 2016;
# inputs        : GTiff files of yield losses for Tanzania calculated using
#               : RICEPEST;
# outputs       : Line graphs of disease progress for each time slice and
#               : scenario;
# remarks 1     : ;
# Licence:      : GPL2;
################################################################################

# Libraries --------------------------------------------------------------------
library(raster)
library(ggplot2)
library(grid)
library(scales)
library(extrafont)
library(wesanderson)
library(ggthemes)

loadfonts(device = "postscript")

# Download data ----------------------------------------------------------------

source("Functions/Get_Data.R")

# Load data --------------------------------------------------------------------

# leaf blast
tz_base_lb <- stack(list.files(path = "../Data/base/",
                               pattern = ".blast_[[:digit:]]{3}.tif$",
                               full.names = TRUE))
tz_base_lb[tz_base_lb == -9999] <- NA

tz_2030_a2_lb <- stack(list.files(path = "../Data/a230/",
                                  pattern = ".blast_[[:digit:]]{3}.tif$",
                                  full.names = TRUE))
tz_2030_a2_lb[tz_2030_a2_lb == -9999] <- NA
tz_2050_a2_lb <- stack(list.files(path = "../Data/a250/",
                                  pattern = ".blast_[[:digit:]]{3}.tif$",
                                  full.names = TRUE))
tz_2050_a2_lb[tz_2050_a2_lb == -9999] <- NA

tz_2030_ab_lb <- stack(list.files(path = "../Data/ab30/",
                                  pattern = ".blast_[[:digit:]]{3}.tif$",
                                  full.names = TRUE))
tz_2030_ab_lb[tz_2030_ab_lb == -9999] <- NA
tz_2050_ab_lb <- stack(list.files(path = "../Data/ab50/",
                                  pattern = ".blast_[[:digit:]]{3}.tif$",
                                  full.names = TRUE))
tz_2050_ab_lb[tz_2050_ab_lb == -9999] <- NA

tz_2030_b1_lb <- stack(list.files(path = "../Data/b130/",
                                  pattern = ".blast_[[:digit:]]{3}.tif$",
                                  full.names = TRUE)) # B1 2030
tz_2030_b1_lb[tz_2030_b1_lb == -9999] <- NA
tz_2050_b1_lb <- stack(list.files(path = "../Data/b150/",
                                  pattern = ".blast_[[:digit:]]{3}.tif$",
                                  full.names = TRUE)) # B1 2050
tz_2050_b1_lb[tz_2050_b1_lb == -9999] <- NA

# bacterial blight
tz_base_bb <- stack(list.files(path = "../Data/base/",
                               pattern = ".bblight_[[:digit:]]{3}.tif$",
                               full.names = TRUE))
tz_base_bb[tz_base_bb == -9999] <- NA

tz_2030_a2_bb <- stack(list.files(path = "../Data/a230/",
                                  pattern = ".bblight_[[:digit:]]{3}.tif$",
                                  full.names = TRUE))
tz_2030_a2_bb[tz_2030_a2_bb == -9999] <- NA
tz_2050_a2_bb <- stack(list.files(path = "../Data/a250/",
                                  pattern = ".bblight_[[:digit:]]{3}.tif$",
                                  full.names = TRUE))
tz_2050_a2_bb[tz_2050_a2_bb == -9999] <- NA

tz_2030_ab_bb <- stack(list.files(path = "../Data/ab30/",
                                  pattern = ".bblight_[[:digit:]]{3}.tif$",
                                  full.names = TRUE))
tz_2030_ab_bb[tz_2030_ab_bb == -9999] <- NA
tz_2050_ab_bb <- stack(list.files(path = "../Data/ab50/",
                                  pattern = ".bblight_[[:digit:]]{3}.tif$",
                                  full.names = TRUE))
tz_2050_ab_bb[tz_2050_ab_bb == -9999] <- NA

tz_2030_b1_bb <- stack(list.files(path = "../Data/b130/",
                                  pattern = ".bblight_[[:digit:]]{3}.tif$",
                                  full.names = TRUE))
tz_2030_b1_bb[tz_2030_b1_bb == -9999] <- NA
tz_2050_b1_bb <- stack(list.files(path = "../Data/b150/",
                                  pattern = ".bblight_[[:digit:]]{3}.tif$",
                                  full.names = TRUE))
tz_2050_b1_bb[tz_2050_b1_bb == -9999] <- NA


# Data munging -----------------------------------------------------------------
lb <- data.frame(values(tz_base_lb),
                 values(tz_2030_a2_lb),
                 values(tz_2050_a2_lb),
                 values(tz_2030_ab_lb),
                 values(tz_2050_ab_lb),
                 values(tz_2030_b1_lb),
                 values(tz_2050_b1_lb))
bb <- data.frame(values(tz_base_bb),
                 values(tz_2030_a2_bb),
                 values(tz_2050_a2_bb),
                 values(tz_2030_ab_bb),
                 values(tz_2050_ab_bb),
                 values(tz_2030_b1_bb),
                 values(tz_2050_b1_bb))

# create matrix of average leaf blast lesion coverage for all of Tanzania,
# by day of growing season
lb_avg <- apply(lb, 2, mean, na.rm = TRUE)
# create matrix of average bacterial leaf blight lesion coverage for all of
# Tanzania, by day of growing season
bb_avg <- apply(bb, 2, mean, na.rm = TRUE)

x <- c(rep("Base", 96),
       rep("A2", 192),
       rep("A1B", 192),
       rep("B1", 192)) # Create vector of emission scenario
y <- c(rep(2000, 96),
       rep(2030, 96),
       rep(2050, 96),
       rep(2030, 96),
       rep(2050, 96),
       rep(2030, 96),
       rep(2050,96)) # Create vector of time-slice midpoint year
# Create vector of days in growing season from transplanting
z <- c(as.numeric(rep(25:120, 7)))
# Create vector of days in growing season from transplanting
z <- c(as.numeric(rep(25:120, 7)))

# Combind the vectors into one dataframe for ggplot2
lb_avg <- data.frame(x, y, z, lb_avg)
bb_avg <- data.frame(x, y, z, bb_avg)

# Assign names to leaf blast data frame
names(lb_avg) <- c("Scenario", "Time.Slice", "Day", "Leaf.Blast")
# Assign names to bacterial blight data frame
names(bb_avg) <- c("Scenario", "Time.Slice", "Day", "Bacterial.Blight")
# Assign sensible row names for dataframe
row.names(lb_avg) <- row.names(bb_avg) <- 1:nrow(lb_avg)

# Data visualisation ----------------------------------------------------------

figure_2 <- ggplot(lb_avg, aes(x = Day, y = Leaf.Blast, group = Scenario)) +
  geom_line(aes(colour = as.factor(Scenario), linetype = as.factor(Scenario)),
            size = 1) +
  scale_x_continuous("Day of Season", limits = c(25, 125)) +
  scale_y_continuous("Leaf Coverage by Leaf Blast Lesions (%)",
                     limits = c(0, 40)) +
  scale_linetype_discrete("Emission\nScenario") +
  scale_colour_manual("Emission\nScenario", values = wes_palette("Moonrise3")) +
  theme_few() +
  theme(axis.text.x = element_text(size = 7, angle = 90, hjust = 1, vjust = 0.5,
                                   family = "Helvetica"),
        axis.title.x = element_text(size = 8, family = "Helvetica"),
        axis.title.y = element_text(size = 7, angle = 90, family = "Helvetica"),
        axis.text.y = element_text(size = 7, family = "Helvetica"),
        legend.position = c(0.11, 0.83),
        legend.background = element_rect(fill = "white", colour = "black"),
        legend.key.width = unit(2.5, "mm"),
        strip.text.x = element_text(size = 7, family = "Helvetica"),
        legend.key.height = unit(2.5, "mm"),
        legend.text = element_text(size = 6, family = "Helvetica"),
        legend.title = element_text(size = 6, family = "Helvetica"),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "lines")) +
  facet_grid(. ~ Time.Slice)
ggsave("../Latex/Figures/Fig2.eps", width = 84, height = 84, units = "mm")

# Bacterial blight graph
figure_3 <- ggplot(bb_avg, aes(x = Day, y = Bacterial.Blight,
                               group = Scenario)) +
  geom_line(aes(colour = as.factor(Scenario), linetype = as.factor(Scenario)),
            size = 1) +
  scale_x_continuous("Day of Season", limits = c(25, 125)) +
  scale_y_continuous("Leaf Coverage by Bacterial Leaf Blight Lesions (%)",
                     limits = c(0, 40)) +
  scale_linetype_discrete("Emission\nScenario") +
  scale_colour_manual("Emission\nScenario", values = wes_palette("Moonrise3")) +
  theme_few() +
  theme(axis.text.x = element_text(size = 7, angle = 90, hjust = 1, vjust = 0.5),
        axis.title.x = element_text(size = 8, family = "Helvetica"),
        axis.title.y = element_text(size = 7, angle = 90, family = "Helvetica"),
        axis.text.y = element_text(size = 7),
        legend.position = c(0.11, 0.83),
        legend.background = element_rect(fill = "white", colour = "black"),
        legend.key.width = unit(2.5, "mm"),
        strip.text.x = element_text(size = 7, family = "Helvetica"),
        legend.key.height = unit(2.5, "mm"),
        legend.text = element_text(size = 6, family = "Helvetica"),
        legend.title = element_text(size = 6, family = "Helvetica"),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "lines")) +
  facet_grid(. ~ Time.Slice)
ggsave("../LaTeX/Figures/Fig3.eps", width = 84, height = 84, units = "mm")

# eos
