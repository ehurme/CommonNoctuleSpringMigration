#################################
# 12_departure_conditions.R
# Description: get environmental data at capture locations 
#  at sunset across the duration of the tagging
# Outputs:
# Figure S5: Departure date and environmental conditions
#################################

# Load libraries and functions
library(pacman)
p_load(tidyverse, ggplot2, terra, ggpubr)
source("./src/consolidated_functions.R")
source("./src/calculate_wind_features.R")
source("./src/extract_environmental_data.R")

# Load data
load("./Data/rdata/spring_swiss_migration_steps_full.robj")
load("./Data/rdata/behavior_thresholds.robj")
load("./Data/rdata/migration_metrics.robj")
load("./Data/rdata/n_day_env.robj")

# Compute Average Bearing
avg_bearing <- circ_mean(na.omit(n_day$bearing), 360)

# ECMWF Environmental Data Files
path = "./Data/ECMWF/"
files <- list.files(path,
                    pattern = "*europe-full.nc")
files <- files[which(nchar(files) == 30)]

# Capture Location
## Extract median latitude and longitude of capture locations
lat_long <- n_df %>% filter(is_capture_point == TRUE) %>% 
  dplyr::select(location_long, location_lat)
latitude <- lat_long$location_lat |> median()
longitude <- lat_long$location_long |> median()

# Dates of Interest
## Identify unique departure dates and their range
dates <- unique(date(departure_time$crossing_time)) |> na.omit()
range(yday(dates)) |> diff()
min_day <- min(dates)
max_day <- max(dates)
temp_dates <- seq.Date(min_day, max_day, by = 1)

# Extract Environmental Data
## Loop through each year to extract nightly environmental data
years <- 2022:2024
departure_env <- data.frame()
year = 2022
for(year in years){
  temp_dates <- seq.Date(as.Date(paste0(year, "-04-03")), as.Date(paste0(year, "-06-07")), by = 1)
  temp_env <- extract_environmental_data(timestamps = temp_dates+ hours(22),
                                         latitudes = rep(latitude, length(temp_dates)),
                                         longitudes = rep(longitude, length(temp_dates)),
                                         shift_hours = 0, 
                                         path = path, files = files)
  departure_env <- rbind(departure_env, temp_env)
  }

# Compute Wind Features
## Calculate wind speed and direction at 100m altitude
departure_env$windsp100_0 <- sqrt(departure_env$u100_0^2 + departure_env$v100_0^2)
departure_env$winddir100_0 <- wind_dir(u_ms = departure_env$u100_0,
                                       v_ms = departure_env$v100_0)
departure_env$year <- year(departure_env$timestamp)


#####################################################################
## plot departure dates ##
#####################################################################

departure_time$year <- year(departure_time$crossing_time)
departure_time$date <- as.Date(departure_time$crossing_time)
departure_time$doy <- yday(departure_time$date)
departure_time$num <- as.numeric(departure_time$date)
unique(departure_time$date)
unique(departure_env$timestamp |> date())

departure_timing_wind_temp <-
  ggplot(departure_time[!is.na(departure_time$crossing_time),])+
  geom_bar(aes(x = date), fill = "gray", col = "darkgray")+
  geom_segment(data = departure_env,
               aes(x = as.Date(timestamp),
                   y = -2,
                   xend = as.Date(timestamp) + (#windsp100_0 *
                     2 * -cos((90-winddir100_0) / 360 * 2 * pi)),
                   yend = #windsp100_0 *
                     -2 + 2 * -sin((90-winddir100_0) / 360 * 2 * pi),
                   col = windsp100_0
               ),
               arrow = arrow(length = unit(0.075, "cm")) )+
  scale_color_viridis_c(name = "Wind speed (m/s)", option = "A")+
  geom_path(data = departure_env, aes(x = as.Date(timestamp),
                                      y = t2m_0-273.15),
            col = "#D55E00")+
  geom_point(data = departure_env[which(departure_env$tp_0 > 0.0005),],
             aes(x = as.Date(timestamp),
                 y = t2m_0-273.15), col = "#0072B2")+
  scale_y_continuous(name = "# of departures", limits = c(-5, 25),
                     sec.axis = sec_axis(~ .,
                                         name = "Nighttime temperature (\u00B0C)"))+
  facet_wrap(~year, scales = "free_x", ncol = 1)+
  theme_bw()+
  theme(legend.position = "bottom", text = element_text(size = 10))

departure_timing_wind_temp
ggsave(departure_timing_wind_temp,
       filename = "./Figures/Supplementary/FigS5_DepartureTimingWindTemp.png",
       width = 3, height = 6)
