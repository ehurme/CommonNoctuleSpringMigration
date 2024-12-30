# Figure 2 map

## plot components of wind
library(pacman)
p_load(tidyverse, dplyr, ggplot2, sf)

# load( "./Data/migration_steps.robj")
load("./Data/rdata/migration_steps_env_days.robj")

t <- table(n_df$tag_local_identifier, n_df$burst_id)
df <- as.data.frame(t)

# Filter the data frame to find tag_local_identifiers that have at least 4 of the same burst_id
result <- df %>%
  filter(Freq >= 4) %>%
  group_by(Var1)

tag = "120ACB9"
j = 5
b1_df <- n_df[n_df$tag_local_identifier == result$Var1[j] &
                     n_df$burst_id == result$Var2[j],]
b1_df <- b1_df[order(b1_df$timestamp),]
plot(b1_df$longitude, b1_df$latitude, type = "o", asp = 1)

b1 <- n_env[n_env$tag_local_identifier ==  result$Var1[j] &
              n_env$burst_id == result$Var2[j],]

b1$timestamp[4] <- b1$timestamp_end[1]
b1$radius <- b1_df$sigfox_computed_location_radius[2:5]

new_coordinates <- function(x0, y0, d, b){
  b_rad = b*pi/180
  x <- x0 + d * cos(b_rad)
  y <- y0 + d * sin(b_rad)
  
  return(c(x,y))
}

xy <- data.frame(x = 0, y = 0,
                 wx = rep(NA,4), wy = NA,
                 ws_x = NA, ws_y = NA,
                 cw_x = NA, cw_y = NA,
                 as_x = NA, as_y = NA,
                 x1 = NA, y1 = NA, label = paste0("t", 1:4),
                 time = c(b1$timestamp[1], 
                          b1$timestamp[1] + cumsum(b1$diff_time[1:3])*60),
                 radius = b1_df$sigfox_computed_location_radius)
xy
for(i in 1:3){
  
  # wind speed
  wind_dir <- b1$winddir100_0[i] + 180
  wind_dir <- (wind_dir + 360) %% 360
  temp_wind <- new_coordinates(x = xy[i,1], y = xy[i,2],
                               d = b1$windsp100_0[i] * b1$diff_time[i] * 60,
                               b = wind_dir)
  xy[i,3:4] <- temp_wind
  
  # wind support
  temp_ws <- new_coordinates(x = xy[i,1], y = xy[i,2],
                             d = b1$ws100_0[i]* b1$diff_time[i] * 60,
                             b = b1$bearing[i])
  xy[i,5:6] <- temp_ws
  
  # cross wind
  cw_dir <- b1$bearing[i] + 90
  cw_dir <- (cw_dir + 360) %% 360
  temp_cw <- new_coordinates(x = xy[i,5], y = xy[i,6],
                             d = b1$cw100_0[i]* b1$diff_time[i] * 60,
                             b = cw_dir)
  xy[i,7:8] <- temp_cw
  
  # airspeed
  temp_as <- new_coordinates(x = xy[i,9], y = xy[i,10],
                             d = b1$airspeed100_0[i]* b1$diff_time[i] * 60,
                             b = b1$bearing[i])
  xy[i,9:10] <- temp_as
  # movement
  temp <- new_coordinates(x = xy[i,1], y = xy[i,2],
                          d = b1$ground_sp[i]* b1$diff_time[i] * 60,
                          b = b1$bearing[i])
  xy[i, 11:12] <- temp
  xy[i+1, 1:2] <- temp
  
}
xy

ggplot(xy[1:4,])+
  geom_path(aes(x,y), lwd = 2)+
  geom_point(aes(x,y), col = 1, size = 5)+
  # ground speed
  geom_segment(aes(x,y, xend = x1, yend = y1),
               arrow = grid::arrow(length = unit(0.25, units = "cm")),
               col = 1, lwd = 1)+
  # wind
  geom_segment(aes(x,y, xend = wx, yend = wy),
               arrow = grid::arrow(length = unit(0.25, units = "cm")),
               col = "#0072B2", lwd = 1)+
  # airspeed
  geom_segment(aes(x = wx, y = wy, xend = x1, yend = y1),
               arrow = grid::arrow(length = unit(0.25, units = "cm")),
               col = "#009E73", lwd = 1)+
  # cross wind
  geom_segment(aes(x = ws_x, y = ws_y, xend = wx, yend = wy),
               arrow = grid::arrow(length = unit(0.25, units = "cm")),
               col = "#D55E00", lwd = 1)+
  # wind support
  geom_segment(aes(x-shift_ws,y-shift_ws, xend = ws_x-shift_ws, yend = ws_y-shift_ws),
               arrow = grid::arrow(length = unit(0.25, units = "cm")),
               col = "#F0E442", lwd = 1)+
  geom_text(aes(x,y,label = format(time, "%H:%M:%S")), nudge_x = -7000, nudge_y = 3000, size = 2)+
  theme_minimal(base_size = 11)+
  coord_equal()+
  xlab("Longitude (UTM)")+
  ylab("Latitude (UTM)") -> p_windmap
print(p_windmap_b3)

save(IDs, xy, shift_ws, p_windmap, file = "./Data/Fig2_windmap.robj")
ggsave(p_windmap,
       filename = "./Figures/Fig2/fig2_windmap.png")

# ggsave(
p_windmap +
  gg_circle(r = xy$radius[1], xc = xy$x[1], yc = xy$y[1])+
  gg_circle(r = xy$radius[2], xc = xy$x[2], yc = xy$y[2])+
  gg_circle(r = xy$radius[3], xc = xy$x[3], yc = xy$y[3])+
  gg_circle(r = xy$radius[4], xc = xy$x[4], yc = xy$y[4])+
  ggtitle(paste0(result$Var1[j], ", burst: ", result$Var2[j]))#,
#      filename = paste0("./../../../Dropbox/MPI/Noctule/Plots/Figures/Fig2/wind_tracks/",
#                                     result$Var1[j], "_burst", result$Var2[j],  ".png"))


