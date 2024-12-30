#######################################
# Script: 4_regularize_tracks.R
# Purpose: Regularize animal movement tracks for consistent temporal intervals.
#######################################

# Consolidated Functions
source("./src/dailydistance.R")

# Load Required Libraries
library(pacman)
p_load(dplyr, lubridate, sf, ggplot2)

# Load Data
load("./Data/rdata/spring_swiss_migration_steps_full.robj") # n_df

# summarize to daily distance
n_day <- {}
n_day <- dailydistance(n_df[which(n_df$tag_fell_off == FALSE),])

# remove irregular data
## the last vedba values for two bats that reset the count
n_day$diff_vedba[which(n_day$diff_vedba < 0)] <- NA

## remove distances separated by less than one day
n_day$distance[which(n_day$diff_time < 0.8)] <- NA

# Add Date-Based Features
n_day$date <- round_date(n_day$time, unit = "days")
n_day$year <- year(n_day$date)
n_day$doy <- yday(n_day$date)

# Calculate Time Intervals Between Consecutive Data Points
n_day$time_int <- NA
IDs <- n_day$ID |> unique()
i = 1
for(i in 1:length(IDs)){
  idx <- which(n_day$ID == IDs[i])
  b <- n_day[idx,]  
  b <- b[order(b$time),]
  n_day$time_int[idx] <- c(as.numeric(diff(date(b$time))), NA)
}

## save data
save(n_day, file = "./Data/rdata/n_day.robj")