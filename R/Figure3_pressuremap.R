# Figure 3 pressure map

# pressure wave
#
library(pacman)
p_load(tidyverse, dplyr,
       lubridate,
       sf, terra, rnaturalearth,
       ggplot2, ggpubr)

load("./Data/rdata/n_day.robj")

date <- ymd("2023-01-01")+124
r <- terra::rast(paste0("./Data/ECMWF/",date, "_", 2023, "_europe-full.nc"))

# Define time points
t_2 <- ymd_hms("2023-05-02 00:00:00")
t_1 <- ymd_hms("2023-05-03 00:00:00")
t0 <- ymd_hms("2023-05-04 00:00:00")
t1 <- ymd_hms("2023-05-05 00:00:00")
t2 <- ymd_hms("2023-05-06 00:00:00")

# Function to extract raster and points for a given time point
extract_raster_points <- function(r, n_day, t, buffer_hours = 12) {
  raster_layer <- {}
  try({raster_layer <- r[[which(time(r) == t)[1]]]})
  points <- n_day %>% filter(abs(difftime(time, t, units = "hours")) < buffer_hours)
  list(raster = raster_layer, points = points)
}

# Extract data for each time point
data_t_1 <- extract_raster_points(r, n_day, t_1)
data_t0 <- extract_raster_points(r, n_day, t0)
data_t1 <- extract_raster_points(r, n_day, t1)
data_t2 <- extract_raster_points(r, n_day, t2)

path0 <- rbind(data_t0$points, data_t_1$points)
path1 <- rbind(data_t0$points, data_t1$points)
path2 <- rbind(data_t1$points, data_t2$points)

# Convert raster to data frame for ggplot
raster_to_df <- function(raster) {
  as.data.frame(raster, xy = TRUE)
}

raster_t0_df <- raster_to_df(data_t0$raster)
names(raster_t0_df) <- c("x", "y", "msl")
raster_t1_df <- raster_to_df(data_t1$raster)
names(raster_t1_df) <- c("x", "y", "msl")
raster_t2_df <- raster_to_df(data_t2$raster)
names(raster_t2_df) <- c("x", "y", "msl")

min_rast <- min(c(raster_t0_df$msl, raster_t1_df$msl, raster_t2_df$msl))/100
max_rast <- max(c(raster_t0_df$msl, raster_t1_df$msl, raster_t2_df$msl))/100

extent <- ext(r[[1]])
buffer = -0.5
xmin = extent[1]-buffer
xmax = extent[2]+buffer
ymin = extent[3]-buffer
ymax = extent[4]+buffer
point_size = 1

msl_breaks = c(1015, 1020, 1025)
date_label <- data.frame(date = c("2023-05-04", "2023-05-05", "2023-05-06"),
                         x = rep(16,3), y = rep(46,3))
p_t0 <- ggplot() +
  geom_raster(data = raster_t0_df, aes(x = x, y = y, fill = msl/100)) +
  geom_sf(data = ne_countries(scale = 10, returnclass = "sf"),
          color = "black", fill = NA) +
  scale_fill_viridis_c(name = "MSL pressure (hPa)", option = "A",
                       limits = c(min_rast, max_rast), breaks = msl_breaks)+
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax))+
  geom_path(data = path0, aes(x = lon, y = lat, group = ID), color = "gray")+
  geom_point(data = data_t0$points, aes(x = lon, y = lat), color = "white",
             size = point_size) +
  geom_point(data = data_t_1$points, aes(x = lon, y = lat), color = "gray50",
             size = point_size) +
  geom_text(data = date_label[1,], aes(x = x, y = y, label = date),
            col = "white", size = 3)+
  xlab("Longitude") + ylab("Latitude")+
  theme_minimal()+
  guides(
    fill = guide_colorbar(
      direction = "horizontal",
      title.position = "top",
      label.position = "bottom",
      title.hjust = 0,#.5,
      label.hjust = 0,
      nrow = 1,
      theme = theme(legend.key.height = unit(0.5, "lines"),
                    legend.key.width = unit(5, "lines"))
    )
  )+
  scale_y_continuous(breaks = c(45, 47, 49, 51, 53))+
  scale_x_continuous(breaks = c(8, 11, 14, 17))
p_t0

# Plot for t1
p_t1 <- ggplot() +
  geom_raster(data = raster_t1_df, aes(x = x, y = y, fill = msl/100)) +
  geom_sf(data = ne_countries(scale = 10, returnclass = "sf"), color = "black", fill = NA) +
  scale_fill_viridis_c(name = "MSL pressure (hPa)", option = "A",
                       limits = c(min_rast, max_rast), breaks = msl_breaks)+
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax))+
  geom_path(data = path1, aes(x = lon, y = lat, group = ID), color = "gray") +
  geom_point(data = data_t0$points, aes(x = lon, y = lat), color = "gray50",
             size = point_size) +
  geom_point(data = data_t1$points, aes(x = lon, y = lat), color = "white",
             size = point_size) +
  geom_text(data = date_label[2,], aes(x = x, y = y, label = date),
            col = "white", size = 3)+
  xlab("Longitude") + ylab("Latitude") +
  theme_minimal()+
  guides(
    fill = guide_colorbar(
      direction = "horizontal",
      title.position = "top",
      label.position = "bottom",
      title.hjust = 0,#.5,
      label.hjust = 0,
      nrow = 1,
      theme = theme(legend.key.height = unit(0.5, "lines"),
                    legend.key.width = unit(5, "lines"))
    )
  )+
  scale_y_continuous(breaks = c(45, 47, 49, 51, 53))+
  scale_x_continuous(breaks = c(8, 11, 14, 17))
p_t1

# Plot for t2
p_t2 <- ggplot() +
  geom_raster(data = raster_t2_df, aes(x = x, y = y, fill = msl/100)) +
  geom_sf(data = ne_countries(scale = 10, returnclass = "sf"), color = "black", fill = NA) +
  scale_fill_viridis_c(name = "MSL pressure (hPa)", option = "A",
                       limits = c(min_rast, max_rast), breaks = msl_breaks)+
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax))+
  geom_path(data = path2, aes(x = lon, y = lat, group = ID), color = "gray") +
  geom_point(data = data_t1$points, aes(x = lon, y = lat), color = "gray50",
             size = point_size) +
  geom_point(data = data_t2$points, aes(x = lon, y = lat), color = "white",
             size = point_size) +
  geom_text(data = date_label[3,], aes(x = x, y = y, label = date),
            col = "white", size = 3)+
  xlab("Longitude") + ylab("Latitude")+
  theme_minimal()+
  guides(
    fill = guide_colorbar(
      direction = "horizontal",
      title.position = "top",
      label.position = "bottom",
      title.hjust = 0,
      label.hjust = 0,
      nrow = 1,
      theme = theme(legend.key.height = unit(0.5, "lines"),
                    legend.key.width = unit(15, "lines"))
    )
  )+
  scale_y_continuous(breaks = c(45, 47, 49, 51, 53))+
  scale_x_continuous(breaks = c(8, 11, 14, 17))
p_t2

combined_pressure_plot <- ggarrange(p_t0+theme(title = element_text(size = 5*14/5/.pt),
                                               legend.title = element_text(size = 7*14/5/.pt),
                                               legend.text = element_text(size = 6*14/5/.pt),
                                               axis.text=element_text(size=7*14/5/.pt),
                                               axis.title=element_text(size=9*14/5/.pt)),
                                    p_t1+theme(title = element_text(size = 5*14/5/.pt),
                                               legend.title = element_text(size = 7*14/5/.pt),
                                               legend.text = element_text(size = 6*14/5/.pt),
                                               axis.text=element_text(size=7*14/5/.pt),
                                               axis.title=element_text(size=9*14/5/.pt)),
                                    p_t2+theme(title = element_text(size = 5*14/5/.pt),
                                               legend.title = element_text(size = 7*14/5/.pt),
                                               legend.text = element_text(size = 6*14/5/.pt),
                                               axis.text=element_text(size=7*14/5/.pt),
                                               axis.title=element_text(size=9*14/5/.pt)),
                                    align = "h", ncol = 3, common.legend = TRUE, legend = "bottom")
combined_pressure_plot
ggsave(combined_pressure_plot,
       filename = "./Figures/Fig3/Fig3_pressuremap.png",
       width = 6, height = 2)
save(combined_pressure_plot,
     extent, n_day, r, buffer, date, max_rast, min_rast,
     t_2, t_1, t0, t1, t2,
     data_t0, data_t1, data_t2,
     p_t0, p_t1, p_t2,
     path0, path1, path2,
     raster_t0_df, raster_t1_df, raster_t2_df,
     file = "./Data/rdata/fig3_pressureplot.robj")
