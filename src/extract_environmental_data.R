#######################################
# Function: Extract Environmental Data
# Purpose: Extract environmental variables at specified times for multiple timestamps and locations.
#######################################

# Function to extract environmental data
extract_environmental_data <- function(timestamps, latitudes, longitudes, shift_hours,
                                       path, files) {
  require(terra)
  require(lubridate)
  
  # Check if the lengths of inputs are valid
  if (length(latitudes) != length(longitudes)) {
    stop("The length of latitudes and longitudes vectors must be the same.")
  }
  
  if (length(timestamps) != length(latitudes)) {
    stop("The length of timestamps must match the number of location points provided.")
  }
  
  # Adjust timestamps based on the shift in hours
  new_timestamps <- as.POSIXct(timestamps + shift_hours * 3600,
                               tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
  
  # Initialize progress bar
  pb <- txtProgressBar(min = 0, max = length(timestamps), style = 3)
  
  # Data frame to store extracted environmental data
  all_extracted_data <- data.frame()
  
  # Loop over each timestamp to find matching files and extract data
  for (i in 1:length(timestamps)) {
    # Update progress bar
    setTxtProgressBar(pb, i)
    
    # Extract date from timestamp
    timestamp_date <- format(new_timestamps[i], "%Y-%m-%d")
    
    # Find matching file for the given date
    matching_file <- grep(timestamp_date, files, value = TRUE)
    if (length(matching_file) == 0) {
      next  # Skip if no matching file is found
    }
    
    # Load the raster file
    env_raster <- rast(file.path(path, matching_file[which.min(nchar(matching_file))]))
    
    # Prepare coordinates for extraction
    coords <- cbind(longitudes[i], latitudes[i])
    
    # Find the time index in the raster corresponding to the timestamp
    idx <- which(terra::time(env_raster) == round_date(new_timestamps[i], unit = "hours"))
    
    # Handle cases where no matching time index is found
    j <- 1
    while (length(idx) == 0 && j <= 4) {
      timestamp_date <- format(timestamps[i] + 24 * 3600 * j, "%Y-%m-%d")
      matching_file <- grep(timestamp_date, files, value = TRUE)
      if (length(matching_file) > 0) {
        env_raster <- rast(file.path(path, matching_file))
        idx <- which(terra::time(env_raster) == round_date(new_timestamps[i], unit = "hours"))
      }
      j <- j + 1
    }
    
    # Extract environmental data for the current timestamp and location
    try({
      env_extract <- raster::extract(env_raster[[idx]], matrix(coords, ncol = 2), method = 'simple')
      
      # Rename columns to indicate the shift in hours
      names(env_extract) <- paste0(gsub("_\\d+$", "", names(env_extract)), "_", shift_hours)
      
      # Add timestamp and location to the extracted data
      if (nrow(env_extract) > 0) {
        env_extract$timestamp <- timestamps[i]
        env_extract$latitude <- latitudes[i]
        env_extract$longitude <- longitudes[i]
        
        # Append extracted data to the result data frame
        all_extracted_data <- rbind(all_extracted_data, env_extract)
      }
    })
  }
  
  # Close the progress bar
  close(pb)
  
  # Return the combined data frame
  return(all_extracted_data)
}

#######################################
# End of Function
#######################################
