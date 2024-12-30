##############################
# 9_interpolate_migrations.R
# Purpose: Interpolate migration paths and calculate environmental metrics along the routes.
##############################

# Load libraries and functions
library(pacman)
p_load(tidyverse,
       terra,
       geosphere, 
       suncalc)
source("./src/consolidated_functions.R")
source("./src/calculate_wind_features.R")
source("./src/extract_environmental_data.R")

# Load data
load("./Data/rdata/migration_metrics.robj")

# Filter migrating bats
m_idx <- which(n_day$migrating == "migrating" & n_day$time_int == 1)

# Initialize Data Frame for Interpolated Migration
migration_interp <- data.frame()
step_km <- 30

# Interpolate Migration Paths
for(i in m_idx) {
  # Ensure that the next point is for the same ID
  if(i < nrow(n_day) && n_day$ID[i] == n_day$ID[i + 1]) {
    distance <- distRhumb(c(n_day$lon[i], n_day$lat[i]), 
                          c(n_day$lon[i + 1], n_day$lat[i + 1])) / 1000
    num_steps <- ceiling(distance / step_km)
    int <- gcIntermediate(p1 = c(n_day$lon[i], n_day$lat[i]), 
                          p2 = c(n_day$lon[i + 1], n_day$lat[i + 1]),
                          n = num_steps, addStartEnd = TRUE)
    
    start <- n_day$time[i]
    end <- n_day$time[i + 1]
    times <- seq.POSIXt(round.POSIXt(start, units = "hour"),
                        round.POSIXt(end, units = "hour"),
                        by = "1 hour")
    
    df <- data.frame(location_lat = rep(int[,2], each = length(times)),
                     location_long = rep(int[,1], each = length(times)),
                     timestamp = rep(times, each = nrow(int)),
                     tag_local_identifier = n_day$ID[i],
                     burst = n_day$burst_id[i],
                     distance)
    # get bearing
    df$bearing <- calculate_bearing(n_day$lon[i], n_day$lat[i],
                                    n_day$lon[i + 1], n_day$lat[i + 1])
    
    df <- determine_day_night(df)
    
    night_times <- df[df$day_night == "Night",1:7]
    
    # Add to migration_interp data frame
    migration_interp <- rbind(migration_interp, night_times)
  }
}

# Extract environmental data
path = "./Data/ECMWF/"
files <- list.files(path,
                    pattern = "*europe-full.nc")

temp <- extract_environmental_data(timestamps = migration_interp$timestamp,
                                   latitudes = migration_interp$location_lat,
                                   longitudes = migration_interp$location_long,
                                   shift_hours = 0, path = path, files = files)

migration_interp <- migration_interp |>
  mutate(latitude = location_lat, longitude = location_long,
         ID = tag_local_identifier, burst_id = burst)
migration_interp$msl_0 <- temp$msl_0
migration_interp$tcc_0 <- temp$tcc_0
migration_interp$tp_0 <- temp$tp_0
migration_interp$t2m_0 <- temp$t2m_0
migration_interp$u10_0 <- temp$u10_0
migration_interp$v10_0 <- temp$v10_0
migration_interp$u100_0 <- temp$u100_0
migration_interp$v100_0 <- temp$v100_0
migration_interp$diff_time <- 1

# Calculate Wind Features
migration_interp <- calculate_wind_features(migration_interp,
                                            "u100", "v100",
                                            "windsp100", "winddir100",
                                            "distance",
                                            "diff_time", "bearing",
                                            hours = 0)

# Aggregate Metrics by Individual and Burst
migration_interp |> reframe(b = mean(bearing),
                            d = mean(distance),
                            time = n(),
                            ws = mean(ws100_0),
                            cw = mean(cw100_0),
                            as = mean(airspeed100_0),
                            .by = c(ID, burst_id)) -> m_int

# Merge Interpolated Metrics with Original Data
n_int <- full_join(n_day, m_int[,c(1,2,6:8)], by = c("ID", "burst_id"))

# Save results
save(n_int, m_int, migration_interp,
     file = "./Data/rdata/migration_interp.robj")

