################################
# Figure1c.R
# Purpose: Plot migration tracks on a map with start location.
################################

# Load libraries
library(pacman)
p_load(lubridate, tidyverse, ggplot2, 
       rnaturalearth, 
       sf, 
       move2,
       ggspatial)

# Load data
load("./Data/rdata/spring_swiss_migration_steps_full.robj")

mig_idx <- which(!is.na(n_df$bearing) & n_df$ground_sp > 1)
n_clean <- n_df[which(complete.cases(n_df$location_lat)&
                             n_df$tag_fell_off == FALSE),]
n_clean <- n_clean %>% dplyr::select(tag_local_identifier, 
                                     timestamp, doy,
                                     location_lat, location_long) %>% na.omit
n_clean$points <- st_as_sf(n_clean, 
                           coords = c("location_long", "location_lat"), 
                           crs = 4326)
m <- move2::mt_as_move2(n_clean,
                        coords = c("location_long", "location_lat"),
                        time_column = "timestamp", 
                        track_id_column = "tag_local_identifier",
                        crs = 4326)
m$year <- m$timestamp |> year()

bat_lines <- m[order(m$tag_local_identifier, m$timestamp),] %>%
  mt_track_lines()
bat_lines$year <- NA
bat_lines$n <- NA
for(i in 1:nrow(bat_lines)){
  bat_lines$year[i] <- year(m$timestamp[which(m$tag_local_identifier == 
                                                bat_lines$tag_local_identifier[i])])[1]
  bat_lines$n[i] <- bat_lines$geometry[i] %>% sf::st_coordinates() %>% length  /3
}
bat_lines <- bat_lines[st_is_valid(bat_lines),]
bat_lines <- bat_lines[bat_lines$n > 1,]

start <- data.frame(name = c("Capture\nLocation"),
                    location_lat = c(47.57),
                    location_long = c(8.89))
start$points <- st_as_sf(start, coords = c("location_long", "location_lat"), crs = 4326)
st_crs(start$points)

buffer = 700
# map of tracks
euro_bat_map <-
  ggplot()+
  geom_sf(data = ne_countries(returnclass = "sf", continent = "Europe", 10),
          col = "darkgray", fill = "black")+
  geom_sf_text(data = ne_countries(returnclass = "sf", continent = "Europe", 10),
               aes(label = name_en), col = "white", size = 2, check_overlap = TRUE)+
  #ne_coastline(returnclass = "sf", 50)) +
  theme_linedraw(base_family = "Helvetica") +
  geom_sf(
    data = bat_lines,
    aes(lty = factor(year), group = tag_local_identifier), col = "white")+
  # geom_path(data = n_clean, aes(location_long, location_lat, group = tag_local_identifier))+
  geom_sf(
    data = n_clean$points,
    aes(col = doy), alpha = 1, size = 1.5
  )+
  geom_sf(data = start$points, size = 3, pch = 24, bg = "purple", col = "white")+
  geom_sf_label(data = start$points, aes(label = name),
                nudge_y = -100, nudge_x = 60, size = 3, col = "purple")+
  coord_sf(
    crs = sf::st_crs("+proj=aeqd +lon_0=15 +lat_0=50 +units=km"),
    xlim = c(-buffer, buffer), ylim = c(-buffer, buffer)
  )+
  scale_linetype_manual(name = "Year", values = c(1,2,3))+
  scale_color_viridis_c(name = "Day of the year", option = "turbo", direction = -1)+
  xlab("Longitude")+
  ylab("Latitude")+
  guides(
    alpha = "none",
    linewidth = "none",
    color = guide_colorbar(
      direction = "horizontal",
      title.position = "top",
      label.position = "bottom",
      title.hjust = .5,
      label.hjust = 0.4,
      nrow = 1
    ),
    linetype = guide_legend(direction = "horizontal",
                            title.position = "top",
                            label.position = "bottom",
                            title.hjust = 0.5,
                            label.hjust = 0,
                            nrow = 1))+
  theme(legend.position = c(.8,.25),
        panel.background = element_rect(fill = 'gray60'),
        legend.title = element_text(size = 8*14/5/.pt),
        legend.text = element_text(size = 7*14/5/.pt),
        axis.text=element_text(size=7*14/5/.pt),
        axis.title=element_text(size=9*14/5/.pt),
        legend.key.size = unit(.4, "cm"),
        legend.spacing.y = unit(0.01, 'cm'))
euro_bat_map_scale <- euro_bat_map+
  ggspatial::annotation_scale(location = "tl", width_hint = 0.5, 
                              bar_cols = c("gray", "white"), text_cex = 1, 
                              pad_x = unit(0.2, "in"), text_col = "white")+
  ggspatial::annotation_north_arrow(location = "tl",
                                    pad_x = unit(0.1, "in"), pad_y = unit(0.25, "in"),
                                    style = ggspatial::north_arrow_nautical(fill = c("grey40", "white"),
                                                                            line_col = "grey20",text_family = "ArcherPro Book"))
euro_bat_map_scale

# Save data
save(euro_bat_map_scale, euro_bat_map, n_clean, bat_lines, start, buffer,
     file = "./Data/rdata/Fig1c_basic_map_doy.robj")
