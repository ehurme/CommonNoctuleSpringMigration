#########################################
# Figure1b.R
# Purpose: Visualize flight distances with directional bearings over years.
#########################################

# Load libraries
library(pacman)
p_load(tidyverse, ggplot2, ggpubr, lubridate, circular)

# Load data
load("./Data/rdata/behavior_thresholds.robj")

m <- n_day[which(n_day$behav == "migrating" & n_day$time_int == 1),]
n_day$ID |> unique() |> length()
m$ID |> unique() |> length()

dir <- as.circular(m$bearing, units = "degrees", template = "geographics")
mean.circular(dir, na.rm = TRUE)
var(dir) |> sqrt()
dir22 <- as.circular(m$bearing[m$year == 2022], 
                     units = "degrees", 
                     template = "geographics")
dir23 <- as.circular(m$bearing[m$year == 2023], 
                     units = "degrees", 
                     template = "geographics")
dir24 <- as.circular(m$bearing[m$year == 2024], 
                     units = "degrees", 
                     template = "geographics")
avg_bearing22 <- mean.circular(dir22)
avg_bearing23 <- mean.circular(dir23)
avg_bearing24 <- mean.circular(dir24)

var(dir22) |> sqrt()
var(dir23) |> sqrt()
var(dir24) |> sqrt()

range(dir22, test = TRUE)
range(dir23, test = TRUE)
range(dir24, test = TRUE)

rayleigh.test(dir) 
rayleigh.test(dir22) 
rayleigh.test(dir23)
rayleigh.test(dir24)
watson.two.test(dir22, dir23)
watson.two.test(dir22, dir24)
watson.two.test(dir24, dir23)

average_bearing <- data.frame(year = 2022:2024, 
                              bearing = c(avg_bearing22 |> as.numeric(),
                                          avg_bearing23 |> as.numeric(),
                                          avg_bearing24 |> as.numeric()),
                              value = c(300,300, 300))

p_bearing_dist <- 
  ggplot(m, aes(bearing, distance, col = diff_vedba))+
  geom_point(pch = 16)+
  geom_segment(data = average_bearing, 
               aes(y=0, xend=bearing, yend=value), 
               col = 1, arrow = arrow(length=unit(0.2,"cm")))+
  scale_color_viridis_c(name = "Daily VeDBA (g)")+
  coord_polar()+
  facet_wrap(~year)+
  scale_x_continuous(name = "Bearing", 
                     limits = c(0,360), 
                     breaks = seq(45,360, by = 45))+
  scale_y_continuous(name = "Daily distance (km)")+
  theme_minimal()+theme(legend.position = "bottom")+
  guides(
    color = guide_colorbar(barwidth = 10,
                           direction = "horizontal",
                           title.position = "top",
                           label.position = "bottom",
                           title.hjust = .5,
                           label.hjust = 0.4,
                           nrow = 1
    ))+
  theme(legend.position = "bottom",
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8))
p_bearing_dist

# ggsave("./Figures/Fig1/Fig1b_direction_distance_year.png",
#        height = 8, width = 6)
save(p_bearing_dist, m, dir22, dir23, dir24, average_bearing,
     file = "./Data/rdata/Fig1b_direction_distance_year.robj")