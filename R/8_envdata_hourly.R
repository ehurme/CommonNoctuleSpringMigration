###########################################
# 8_envdata_hourly.R
# Description:
###########################################

# Load libraries and functions
library(pacman)
p_load(data.table,
       lubridate, tidyverse, 
       terra)
source("./src/extract_avg_night_environmental_data.R")
source("./src/calculate_wind_features.R")
source("./src/consolidated_functions.R")

# Load data
load("./Data/rdata/migration_metrics.robj")

# Define Parameters and Preprocess Data
path = "./Data/ECMWF"
n_day$timestamp <- n_day$time
n_day$latitude <- n_day$lat
n_day$longitude <- n_day$lon

# Initialize Environmental Data Table
env_avg_night <- data.table(timestamp = NA, latitude = NA, longitude = NA, ID = NA)
days = 2

# Filter Data for Environmental Extraction
o <- n_day[which(!is.na(n_day$timestamp)),]
timestamp = o$timestamp
latitude = o$latitude
longitude = o$longitude
ID = o$ID

# Extract Average Nightly Environmental Data
## Loop through each day shift (-days to +days)
for(day in -days:days){
  test <- extract_avg_night_environmental_data(timestamps = timestamp,
                                               latitudes = latitude,
                                               longitudes = longitude,
                                               IDs = ID,
                                               shift_hours = 24*day,
                                               path = path)
  env_avg_night <- full_join(env_avg_night, test$average_night_data,
                             by = c("timestamp", "latitude", "longitude", "ID"))
}

# Merge Environmental Data with Original Dataset
n_day_env_avg_night <- full_join(n_day, env_avg_night[2:nrow(env_avg_night),],
                                 by = c("timestamp", "latitude", "longitude", "ID"))

# Calculate Wind Features
n_day_env_avg_night <- calculate_wind_features(n_day_env_avg_night, 
                                               "u100", "v100",
                                               "windsp100", "winddir100",
                                               "distance",
                                               "diff_time", "bearing",
                                               hours = seq(-24*days, 24*days, by = 24))
# Clean column names
n_day_env_avg_night <- n_day_env_avg_night[,-grep("expver", names(n_day_env_avg_night))]

# Save Processed Data
save(n_day_env_avg_night, days, env_avg_night,
     file = "./Data/rdata/n_day_env.robj")

