#######################################
# Script: 3_migration_steps_env_days.R
# Purpose: Extract and process environmental data during migration flights.
#######################################

# Consolidated Functions
source("./src/consolidated_functions.R")
source("./src/extract_environmental_data.R")
source("./src/calculate_wind_features.R")

# Load Required Libraries
library(pacman)
p_load(terra, dplyr, data.table)

# Load Data
load("./Data/rdata/spring_swiss_migration_steps_full.robj") # n_df
load("./Data/rdata/vpm_speed_threshold.robj")

# Define Parameters
path <- "./Data/ECMWF/"
files <- list.files(path, pattern = "*europe-full.nc")
days <- 2

# Prepare Data for Environmental Extraction
n_df$latitude <- n_df$location_lat
n_df$longitude <- n_df$location_long

# Initialize Data Table for Environmental Data
env <- data.table(timestamp = NA, latitude = NA, longitude = NA)

env_idx <- which(n_df$tag_fell_off == FALSE & n_df$vpm > vpm_thresh)

# Extract Environmental Data for Multiple Days
day <- 0
for (day in -days:days) {
  message(sprintf("Processing day shift: %d", day))
  temp <- extract_environmental_data(
    timestamps = n_df$timestamp[env_idx],
    latitudes = n_df$location_lat[env_idx],
    longitudes = n_df$location_long[env_idx],
    shift_hours = 24 * day,
    path = path,
    files = files
  )
  env <- dplyr::full_join(env, temp, by = c("timestamp", "latitude", "longitude"))
}

# Merge Environmental Data with Original Data
n_env <- dplyr::full_join(n_df[env_idx, ], env, 
                          by = c("timestamp", "latitude", "longitude"))

# Calculate Wind Features
n_env <- calculate_wind_features(n_env, 
                                 "u100", "v100", 
                                 "windsp100", "winddir100", 
                                 "distance", "diff_time", "bearing", 
                                 hours = seq(-24 * days, 24 * days, by = 24))
n_env <- calculate_wind_features(n_env, "u10", "v10", 
                                 "windsp10", "winddir10", 
                                 "distance", "diff_time", "bearing", 
                                 hours = seq(-24 * days, 24 * days, by = 24))

# Save Processed Data
save(n_df, n_env, file = "./Data/rdata/migration_steps_env_days.robj")
