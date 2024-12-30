##############################################
# Script: 1_download.data.R
# Purpose: Load and preprocess data for migration study.
##############################################

library(pacman)
p_load(lubridate, tidyverse, data.table, janitor,
       sf)

# Consolidated Functions
source("./src/consolidated_functions.R")

## Download data from movebank
### move2
# study = 4900735626
# n <- move2::movebank_download_study(study_id = study)
# mt_track_id(n) <- "tag_local_identifier"
# n_data <- move2::mt_track_data(n)

# Load Data from Movebank Repository
df <- read.csv("./Data/Common noctule migration SigFox MPIAB.csv") %>% clean_names()
n_data <- read.csv("./Data/Common noctule migration SigFox MPIAB-reference-data.csv") %>% clean_names()

# Fix error in deployment time
n_data$deploy_on_date[which(n_data$tag_id == "12106F8")] <- "2024-04-12 11:00:00.000"

# Adjust time
df <- df %>%
  mutate(timestamp = as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
n_data <- n_data %>%
  mutate(deploy_on_date = as.POSIXct(deploy_on_date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC") - 2*3600) 

## fix 2024 dates
df$timestamp[which(year(df$timestamp) == 2024)] <- df$timestamp[which(year(df$timestamp) == 2024)] - 1*3600
n_data$deploy_on_date[which(year(n_data$deploy_on_date) == 2024)] <-
  n_data$deploy_on_date[which(year(n_data$deploy_on_date) == 2024)] + 1*3600

## daylight savings time adjustment
# +1 hour all data before March 31st
df$timestamp[which(month(df$timestamp) == 3 & day(df$timestamp) < 31)] <-
  df$timestamp[which(month(df$timestamp) == 3 & day(df$timestamp) < 31)] + 1*3600
n_data$deploy_on_date[which(month(n_data$deploy_on_date) == 3 & day(n_data$deploy_on_date) < 31)] <-
  n_data$deploy_on_date[which(month(n_data$deploy_on_date) == 3 & day(n_data$deploy_on_date) < 31)] + 1*3600

# make a data.frame
n_df <- add_capture_data(df = df, capture_df = n_data)

# Determine bursts ####
n_df <- determine_bursts(n_df)

# convert from m/s^2 to mg
n_df$tinyfox_total_vedba_mg <- n_df$tinyfox_total_vedba / (1000/3.9)
n_df <- diff_vedba(n_df) # calculate change in vedba between fixes
n_df$vpm <- n_df$diff_vedba/n_df$diff_time # VeDBA per minute
n_df$vpm[which(n_df$vpm < 0)] <- NA # remove negative VeDBA

# speed
n_df <- diff_dist(n_df)
n_df$ground_sp <- n_df$distance/(n_df$diff_time*60)

# Add sunrise and sunset to the data frame ----
n_df <- determine_day_night(n_df)
n_df$sunrise <- as.POSIXct(n_df$sunrise, origin="1970-01-01")
n_df$sunset <- as.POSIXct(n_df$sunset, origin="1970-01-01")
n_df$noon <- as.POSIXct(n_df$noon, origin="1970-01-01")

n_df$time_to_noon <- difftime(n_df$timestamp, n_df$noon, units = "hours")

# Show the first few rows of the updated data frame
head(n_df)

n_df$hour <- hour(n_df$timestamp)+minute(n_df$timestamp)/60
n_df$doy <- yday(n_df$timestamp)


## Calculate track metrics
## get cumulative distance, straight-line distance, and straightness index
IDs <- unique(n_df$tag_local_identifier)
n_df$n <- NA 
n_df$cumdist <- NA # cummulative distance
n_df$straightdist <- NA # straight-line distance
n_df$str_idx <- NA # straightness index
n_df$bearing <- NA

i = 1
for(i in 1:length(IDs)){
  b_idx <- which(n_df$tag_local_identifier == IDs[i])
  b <- n_df[b_idx,]
  bursts <- unique(b$burst_id)
  j = 2
  for(j in 1:length(bursts)){
    bb_idx <- which(n_df$tag_local_identifier == IDs[i] &
                      n_df$burst_id == bursts[j])
    b1 <- n_df[bb_idx,]
    
    if(nrow(b1) > 0){
      n_df$n[bb_idx] <- length(bb_idx)
      n_df$cumdist[bb_idx] <- sum(b1$distance/1000, na.rm = TRUE)
      n_df$straightdist[bb_idx] <- geosphere::distGeo(c(b1$location_long[1],
                                                        b1$location_lat[1]),
                                                      c(b1$location_long[nrow(b1)],
                                                        b1$location_lat[nrow(b1)]))/1000
      n_df$str_idx[bb_idx] <- n_df$straightdist[bb_idx]/n_df$cumdist[bb_idx]
      
      for(k in 1:(nrow(b1)-1)){
        n_df$bearing[bb_idx[k]] <- calculate_bearing(lon1 = b$location_long[k],
                                                     lat1 = b$location_lat[k],
                                                     lon2 = b$location_long[k+1],
                                                     lat2 = b$location_lat[k+1])
      }
    }
  }
}

summary(n_df)

## Remove duplicate data from re-tagged individuals
### remove data from first deployment as it appear tag fell off right away
# with(n_df[n_df$tag_local_identifier == "120AF1E" | 
#             n_df$tag_local_identifier == "120B2E2",],
#      plot(timestamp, tinyfox_total_vedba, col = factor(tag_local_identifier)))
n_df <- n_df[-which(n_df$tag_local_identifier == "120B2E2"),]

## Mark data as "fell off" based on thresholds
n_df$tag_fell_off <- FALSE
motionless_tag <- 280800*3.9/1000/(60*24)
n_df <- tag_fell_off(df = n_df, 
                     vedba_threshold = motionless_tag*2, 
                     activity_threshold = 0)

# remove males
males <- unique(n_df$tag_local_identifier[n_df$animal_sex == "m"]) %>% na.omit()
m_df <- n_df[which(n_df$tag_local_identifier %in% males),]
n_df <- n_df[-which(n_df$tag_local_identifier %in% males),]

## Remove early burst sensors ----
sensor_filter <- which(n_df$sequence_number < 5 | n_df$tinyfox_activity_percent_last_24h > 70)
n_df[sensor_filter, c("tinyfox_activity_percent_last_24h", 
                      "tinyfox_temperature_max_last_24h", 
                      "tinyfox_temperature_min_last_24h")] <- NA

# Add missing tag
missing <- read.csv("./Data/Tag_120B465.csv")
missing$timestamp <- ymd_hms(missing$timestamp)
missing$sunrise <- ymd_hms(missing$sunrise)
missing$sunset <- ymd_hms(missing$sunset)
missing$noon <- ymd_hms(missing$noon)
missing$time_to_noon <- as.duration(missing$time_to_noon)
n_df <- full_join(n_df, missing)
n_df <- unique(n_df)

# Save data ----
save(df, n_data, m_df, n_df,
     file = "./Data/rdata/spring_swiss_migration_steps_full.robj")

