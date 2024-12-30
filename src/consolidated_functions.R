#######################################
# Consolidated Functions for Data Processing
# File: consolidated_functions.R
# Purpose: Simplifies and consolidates functions for data processing.
#######################################

# Required Libraries
library(dplyr)
library(sf)
library(stringr)
library(geosphere)
library(suncalc)

# Function: Add Capture Data to Main Dataset
add_capture_data <- function(df, capture_df) {
  if (is.null(df$location_lat)) {
    coordinates <- st_coordinates(df$geometry)
    df$location_long <- coordinates[, "X"]  # Longitude
    df$location_lat <- coordinates[, "Y"]   # Latitude
  }
  
  if (is.null(capture_df$deploy_on_latitude)) {
    coordinates <- st_coordinates(capture_df$deploy_on_location)
    capture_df$deploy_on_longitude <- coordinates[, "X"]
    capture_df$deploy_on_latitude <- coordinates[, "Y"]
  }
  
  if (is.null(capture_df$tag_local_identifier)) {
    capture_df$tag_local_identifier <- capture_df$tag_id
  }
  
  if (is.null(capture_df$individual_local_identifier)) {
    capture_df$individual_local_identifier <- capture_df$animal_id
  }
  
  # Prepare Capture Data
  capture_df <- capture_df %>%
    dplyr::select(
      tag_local_identifier,
      individual_local_identifier,
      animal_sex,
      deploy_on_date,
      animal_mass,
      deploy_on_measurements,
      animal_reproductive_condition,
      attachment_type,
      tag_mass_total,
      deploy_on_latitude,
      deploy_on_longitude
    ) %>%
    mutate(
      location_lat = deploy_on_latitude,
      location_long = deploy_on_longitude,
      sex = animal_sex,
      fa = as.numeric(str_extract(deploy_on_measurements, "\\d+\\.?\\d*")),
      timestamp = deploy_on_date
    )
  
  # Create Capture Location Rows
  capture_location_rows <- capture_df %>% mutate(is_capture_point = TRUE)
  
  # Merge Capture Location Rows with Main Data
  merged_df <- bind_rows(df, capture_location_rows) %>% arrange(tag_local_identifier, timestamp)
  
  return(merged_df)
}

# Function to determine if each timestamp is during day or night and add solar times
determine_day_night <- function(df) {
  require(suncalc)
  # Initialize new columns for day/night and solar times
  df$day_night <- NA
  df$sunrise <- NA
  df$sunset <- NA
  df$noon <- NA
  
  # Initialize progress bar
  pb <- txtProgressBar(min = 0, max = nrow(df), style = 3)
  
  # Loop through each row
  for (i in 1:nrow(df)) {
    # Update progress bar
    setTxtProgressBar(pb, i)
    # Ensure that sunrise, sunset, and timestamp are in POSIXct format
    current_time <- as.POSIXct(df$timestamp[i], origin="1970-01-01", tz = "CET")
    current_date <- as.Date(current_time)
    latitude <- df$location_lat[i]
    longitude <- df$location_long[i]
    if(!is.na(latitude)){
      # Get solar position for the current timestamp
      solar_position <- getSunlightPosition(date=current_time, lat=latitude, lon=longitude)
      
      # Use solar altitude to determine day or night
      if (solar_position$altitude > 0) {
        df$day_night[i] <- "Day"
      } else {
        df$day_night[i] <- "Night"
      }
      
      # Get sun times for the current, previous, and next dates
      sun_times_today <- getSunlightTimes(date=current_date, lat=latitude, lon=longitude)
      sun_times_prev <- getSunlightTimes(date=current_date - 1, lat=latitude, lon=longitude)
      sun_times_next <- getSunlightTimes(date=current_date + 1, lat=latitude, lon=longitude)
      
      # Separate candidate sun times for each event
      candidate_sunrise_times <- c(
        as.POSIXct(sun_times_today$sunrise, origin="1970-01-01"),
        as.POSIXct(sun_times_prev$sunrise, origin="1970-01-01"),
        as.POSIXct(sun_times_next$sunrise, origin="1970-01-01")
      )
      
      candidate_sunset_times <- c(
        as.POSIXct(sun_times_today$sunset, origin="1970-01-01"),
        as.POSIXct(sun_times_prev$sunset, origin="1970-01-01"),
        as.POSIXct(sun_times_next$sunset, origin="1970-01-01")
      )
      
      candidate_noon_times <- c(
        as.POSIXct(sun_times_today$solarNoon, origin="1970-01-01"),
        as.POSIXct(sun_times_prev$solarNoon, origin="1970-01-01"),
        as.POSIXct(sun_times_next$solarNoon, origin="1970-01-01")
      )
      
      # Find the nearest sunrise, sunset, and solar noon times
      nearest_sunrise <- candidate_sunrise_times[which.min(abs(difftime(candidate_sunrise_times, current_time, units="mins")))]
      nearest_sunset <- candidate_sunset_times[which.min(abs(difftime(candidate_sunset_times, current_time, units="mins")))]
      nearest_noon <- candidate_noon_times[which.min(abs(difftime(candidate_noon_times, current_time, units="mins")))]
      
      # Save these times to the dataframe
      df$sunrise[i] <- nearest_sunrise
      df$sunset[i] <- nearest_sunset
      df$noon[i] <- nearest_noon
    }
  }
  
  # Close the progress bar
  close(pb)
  
  return(df)
}


determine_bursts <- function(df) {
  # # Convert timestamp to POSIXct type for time-based calculations
  # df$timestamp <- as.POSIXct(df$timestamp, format="%Y-%m-%d %H:%M:%S")
  
  # Sort the dataframe by 'tag_local_identifier' and 'timestamp'
  df <- df |> arrange(tag_local_identifier, timestamp)
  
  # Initialize a new column for burst_id
  df$burst_id <- NA_real_
  
  # Loop through each tag
  for (tag in unique(df$tag_local_identifier)) {
    # Subset data for the current tag
    sub_df <- df |> filter(tag_local_identifier == tag)
    sub_df$burst_id[1] <- 1
    if(nrow(sub_df) > 1){
      # Initialize burst_id for this subset
      burst_id = 1
      
      # Loop through each row to assign burst_id
      for (i in 2:nrow(sub_df)) {
        time_diff <- as.numeric(difftime(sub_df$timestamp[i], sub_df$timestamp[i-1], units="mins"))
        
        # If time difference is more than 60 minutes, it's a new burst
        if(time_diff > 130) {
          burst_id <- burst_id + 1
        }
        
        sub_df$burst_id[i] <- burst_id
      }
    }
    # Update the original dataframe with the burst_ids for this tag
    df[df$tag_local_identifier == tag, ] <- sub_df
  }
  return(df)
}

diff_vedba <- function(df){
  df$diff_vedba <- NA_real_
  df$diff_time <- NA_real_
  # Sort the dataframe by 'tag_local_identifier' and 'timestamp'
  df <- df |> arrange(tag_local_identifier, timestamp)
  
  for (tag in unique(df$tag_local_identifier)) {
    # Subset data for the current tag
    sub_df <- df |> filter(tag_local_identifier == tag)
    sub_df$diff_vedba[1] <- as.numeric(sub_df$tinyfox_total_vedba_mg[1])
    if(nrow(sub_df) > 1){
      # Loop through each row to assign burst_id
      for (i in 2:nrow(sub_df)) {
        vedba_diff <- as.numeric(sub_df$tinyfox_total_vedba_mg[i] - sub_df$tinyfox_total_vedba_mg[i-1])
        sub_df$diff_vedba[i-1] <- vedba_diff
        sub_df$diff_time[i-1] <- difftime(sub_df$timestamp[i], sub_df$timestamp[i-1], units = "mins")
      }
    }
    # Update the original dataframe with the burst_ids for this tag
    df[df$tag_local_identifier == tag, ] <- sub_df
  }
  return(df)
}

# Function to calculate distance between consecutive points for all tags
diff_dist <- function(df) {
  require(geosphere)
  # Initialize a new column for distances
  df$distance <- NA_real_
  df$distance_from_start <- NA_real_
  # Loop through each unique tag
  for (tag in unique(df$tag_local_identifier)) {
    # Subset the data for the current tag
    sub_df <- df[df$tag_local_identifier == tag, ]
    sub_df$distance_from_start[1] <- 0
    
    if(nrow(sub_df) > 1){
      # Sort the data by timestamp if needed
      sub_df <- sub_df[order(sub_df$timestamp), ]
      start <- c(sub_df$location_long[1], sub_df$location_lat[1])
      # Calculate distance for this subset
      for (i in 2:nrow(sub_df)) {
        coord1 <- c(sub_df$location_long[i-1], sub_df$location_lat[i-1])
        coord2 <- c(sub_df$location_long[i], sub_df$location_lat[i])
        
        # Calculate distance
        ## distance is assigned to the first location of a segment
        sub_df$distance[i-1] <- distGeo(coord1, coord2)
        sub_df$distance_from_start[i] <- distGeo(start,coord2)
      }
    }
    # Update the original dataframe with the distances for this tag
    df[df$tag_local_identifier == tag, ] <- sub_df
  }
  return(df)
}

# Function to calculate bearing for sequential points
calculate_bearing_for_sequential_points <- function(df) {
  # Initialize a new column for bearing
  df$bearing <- NA
  
  # Get the unique identifiers
  unique_tags <- unique(df$tag_local_identifier)
  
  # Loop through each unique identifier to calculate bearing
  for(tag in unique_tags) {
    subset_df <- df[df$tag_local_identifier == tag,]
    if(nrow(subset_df) > 1){
      for(i in 1:(nrow(subset_df)-1)) {
        row1 <- subset_df[i,]
        row2 <- subset_df[i+1,]
        
        # Calculate time difference
        time_diff <- as.numeric(difftime(row2$timestamp, row1$timestamp, units = "mins"))
        if(!is.na(time_diff)){
          # If time difference is less than 100, calculate bearing
          if(time_diff < 100) {
            lon1 <- row1$location_long
            lat1 <- row1$location_lat
            lon2 <- row2$location_long
            lat2 <- row2$location_lat
            
            bearing <- calculate_bearing(lon1, lat1, lon2, lat2)
            
            # Check if the subsetting actually returns rows to update
            indices_to_update <- which(df$timestamp == row2$timestamp & df$tag_local_identifier == tag)
            if (sum(indices_to_update) > 0) {
              df$bearing[indices_to_update] <- bearing
            }
          }
        }
      }
    }
  }
  
  return(df)
}



# Function: Calculate Circular Mean
circ_mean <- function(m, int) {
  rad_m <- m * (360 / int) * (pi / 180)
  mean_cos <- mean(cos(rad_m))
  mean_sin <- mean(sin(rad_m))
  x_deg <- atan2(mean_sin, mean_cos) * (180 / pi)
  
  return((x_deg + 360) %% 360 / (360 / int))
}

################# Wind Support Functions

# Function: Calculate Wind Direction
wind_dir <- function(u_ms, v_ms) {
  wind_abs <- sqrt(u_ms^2 + v_ms^2)
  wind_dir_trig_to <- atan2(u_ms / wind_abs, v_ms / wind_abs) * (180 / pi)
  wind_dir_trig_from <- (wind_dir_trig_to + 180) %% 360
  wind_dir_cardinal <- (90 - wind_dir_trig_from + 360) %% 360
  
  return(wind_dir_cardinal)
}

# Function: Calculate Bearing
calculate_bearing <- function(lon1, lat1, lon2, lat2) {
  lat1_rad <- lat1 * (pi / 180)
  lat2_rad <- lat2 * (pi / 180)
  delta_lon <- (lon2 - lon1) * (pi / 180)
  
  bearing <- atan2(
    sin(delta_lon) * cos(lat2_rad),
    cos(lat1_rad) * sin(lat2_rad) - sin(lat1_rad) * cos(lat2_rad) * cos(delta_lon)
  ) * (180 / pi)
  
  return((bearing + 360) %% 360)
}

# Function: Calculate Wind Support
wind_support <- function(u, v, heading) {
  angle <- atan2(u, v) - heading * (pi / 180)
  return(cos(angle) * sqrt(u^2 + v^2))
}

# Function: Calculate Cross Wind
cross_wind <- function(u, v, heading) {
  angle <- atan2(u, v) - heading * (pi / 180)
  return(sin(angle) * sqrt(u^2 + v^2))
}

# Function: Calculate Airspeed
airspeed <- function(ground_speed, ws, cw) {
  return(sqrt((ground_speed - ws)^2 + cw^2))
}

# Function: Detect if Tag Fell Off
tag_fell_off <- function(df, vedba_threshold = 280800 * 3.9 / 1000 / (60 * 24), 
                         activity_threshold = 0) {
  tags <- unique(df$tag_local_identifier)
  df$tag_fell_off <- FALSE # Initialize the new column
  
  for (tag in tags) {
    tag_data_indices <- which(df$tag_local_identifier == tag)
    tag_data <- df[tag_data_indices, ]
    
    if (nrow(tag_data) > 1) {
      # Check if all values are below thresholds
      if (all(tag_data$vpm < vedba_threshold &
              tag_data$tinyfox_activity_percent_last_24h <= activity_threshold, na.rm = TRUE)) {
        df$tag_fell_off[tag_data_indices] <- TRUE
      }
      
      # Check if any value exceeds thresholds
      if (any(tag_data$vpm >= vedba_threshold |
              tag_data$tinyfox_activity_percent_last_24h > activity_threshold, na.rm = TRUE)) {
        # Find the last time above thresholds
        last_time_above_threshold <- max(which(tag_data$vpm >= vedba_threshold |
                                                 tag_data$tinyfox_activity_percent_last_24h > activity_threshold))
        if (length(last_time_above_threshold) > 0) {
          fell_off_index <- last_time_above_threshold
          # Mark as fell off if subsequent values are below thresholds
          if (all(tag_data$vpm[(fell_off_index+1):nrow(tag_data)] < vedba_threshold &
                  tag_data$tinyfox_activity_percent_last_24h[(fell_off_index+1):nrow(tag_data)] <= activity_threshold, na.rm = TRUE)) {
            df$tag_fell_off[tag_data_indices[(fell_off_index+1):nrow(tag_data)]] <- TRUE
          }
        }
      }
      
      # If only the last point is vpm NA, ensure it is marked FALSE
      if (is.na(last(tag_data$vpm)) &&
          df$tag_fell_off[tag_data_indices[length(tag_data_indices)-1]] == FALSE) {
        df$tag_fell_off[tag_data_indices[length(tag_data_indices)]] <- FALSE
      }
    }
  }
  return(df)
}

haversine <- function(lat1, lon1, lat2, lon2) {
  R <- 6371.0  # Radius of Earth in km
  
  dlat <- (lat2 - lat1) * (pi / 180)
  dlon <- (lon2 - lon1) * (pi / 180)
  
  a <- sin(dlat / 2)^2 + cos(lat1 * (pi / 180)) * cos(lat2 * (pi / 180)) * sin(dlon / 2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  
  distance <- R * c
  return(distance)
}


check_trajectory <- function(trajectory, start_lat, start_lon) {
  for (i in 1:nrow(trajectory)) {
    lat <- trajectory[i, 'lat']
    lon <- trajectory[i, 'lon']
    time <- trajectory[i, 'time']
    
    distance <- haversine(start_lat, start_lon, lat, lon)
    # print(distance)
    if (distance > dist_thresh) {
      return(time)  # Return the timestamp when the trajectory crossed the 300 km buffer.
    }
  }
  return(NULL)  # The trajectory stayed within the 100 km buffer.
}

#######################################
# End of Consolidated Functions File
#######################################
