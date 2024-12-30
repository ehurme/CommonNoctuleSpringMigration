#######################################
# Script: 10_clean_data.R
# Purpose: clean data for modeling
#######################################

# Load libraries and functions
library(pacman)
p_load(tidyverse, ggplot2, lubridate, 
       janitor, data.table, dplyr)
source("./src/consolidated_functions.R")
source("./src/calculate_wind_features.R")

# Load data
load("./Data/rdata/migration_metrics.robj")
load("./Data/rdata/n_day_env.robj")

# Define "Busy Nights"
## Calculate the proportion of bats migrating each night
n_day_env_avg_night |> 
  reframe(percent_migrating = length(which(migrating == "migrating"))/n(),
          n = n(), 
          .by = date) -> busy

## Compute Mean Proportion of Migrating Bats
mean_busy <- busy$percent_migrating[busy$percent_migrating > 0] |> mean()

## Label Nights as "Busy" or Not Based on Mean
n_day_env_avg_night$busy_mean <- NA
n_day_env_avg_night$busy_mean[n_day_env_avg_night$date %in% 
                                busy$date[busy$percent_migrating > mean_busy]] <- 1
n_day_env_avg_night$busy_mean[n_day_env_avg_night$date %in% 
                                busy$date[busy$percent_migrating <= mean_busy]] <- 0

# Filter and Clean Data
## Retain rows with complete cases for behavior and environmental variables
m <- n_day_env_avg_night[which(complete.cases(n_day_env_avg_night$behav) &
                                complete.cases(n_day_env_avg_night$t2m_0)),]

## Convert Migration Status to Binary
m$migrating01 <- ifelse(m$migrating == "migrating", 1, 0)

# Calculate Average Wind Support
## Compute Average Distance and Bearing for Migrating Bats
m$mean_distance <- mean(m$distance[m$behav == "migrating" &
                                     m$time_int == 1], na.rm = TRUE) 
m$mean_bearing <- circ_mean(m$bearing[which(m$behav == "migrating" &
                                              m$time_int == 1)], int = 360)
m$mean_time <- 1

## Calculate Wind Features for Averaged Metrics
m <- calculate_wind_features(m, "u100", "v100",
                             "windsp100", "winddir100",
                             "mean_distance",
                             "mean_time", "mean_bearing",
                             seq(-48, 48, by = 24))
m$'avg_ws100_-48' <- m$`ws100_-48`
m$'avg_ws100_-24' <- m$`ws100_-24`
m$avg_ws100_0 <- m$ws100_0
m$avg_ws100_24 <- m$ws100_24
m$avg_ws100_48 <- m$ws100_48

# Calculate Wind Features for Specific Time Offsets
m <- calculate_wind_features(m, "u100", "v100",
                             "windsp100", "winddir100",
                             "distance",
                             "diff_time", "bearing",
                             c(-24,0))

# Create Cleaned Data for Modeling
## Transform and Filter Variables
cm <- m |>
  mutate(migration = ifelse(migrating == "migrating", 1, 0),
         tp = tp_0,
         diff_tp = tp_0 - `tp_-24`,
         t2m = t2m_0,
         diff_t2m = t2m_0 - `t2m_-24`,
         msl = msl_0,
         diff_msl = msl_0 - `msl_-24`,
         tcc = tcc_0,
         diff_tcc = tcc_0 - `tcc_-24`,
         windsp = windsp100_0,
         diff_windsp = windsp100_0 - `windsp100_-24`,
         ws = ws100_0,
         diff_ws = ws100_0 - `ws100_-24`,
         avg_ws = avg_ws100_0,
         diff_avg_ws = avg_ws100_0 - `avg_ws100_-24`,
         year = as.factor(year)) |>
  filter(time_int == 1 & !is.na(ws100_0)) |>
  dplyr::select(ID, migration, behav,
                busy_mean,
                year, date, doy,  day_night,
                distance, diff_vedba,
                tp, t2m, tcc, msl,
                windsp, 
                ws, avg_ws,
                diff_tp, diff_tcc,
                diff_ws, diff_avg_ws,
                diff_msl, diff_t2m,
                diff_windsp) %>% 
  na.omit()

# Save Cleaned Data
save(m, cm, file = "./Data/rdata/clean_data.robj")
