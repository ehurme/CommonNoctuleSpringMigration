# get average environmental variables from the night of migration

# timestamps = as.POSIXct("2023-05-04 21:00:00", tz = "UTC")
# latitudes = 47.57201
# longitudes = 8.894872
# IDs = "120AC91"
# shift_hours = 0
# path = "./../../../Dropbox/MPI/Noctule/Data/EnvData/ECMWFR"
# files <- list.files(path,
#                     pattern = "*europe-full.nc")

combine_columns <- function(df) {
  # Get unique column names
  table_names <- table(names(df))
  unique_names <- unique(names(df))
  
  # Initialize an empty data frame to store combined columns
  combined_df <- data.frame(matrix(ncol = length(unique_names), nrow = table_names[1]))
  names(combined_df) <- unique_names
  
  # Loop through each unique column name
  for (name in unique_names) {
    combined_df[[name]] <- df[, names(df) == name] |> unlist()
  }
  
  return(combined_df)
}

# extract_nighttime_hours(timestamps, latitudes, longitudes)

extract_nighttime_hours <- function(timestamps, latitudes, longitudes) {
  require(lubridate)
  require(suncalc)
  
  # Initialize a list to store nighttime hours
  nighttime_hours_all <- list()
  
  # Loop over each timestamp
  for (i in 1:length(timestamps)) {
    current_time <- as.POSIXct(timestamps[i], tz = "UTC")
    start_of_period <- current_time - hours(24)
    end_of_period <- current_time
    
    # Calculate sunrise and sunset times
    sun_info_today <- getSunlightTimes(date = date(current_time), lat = latitudes[i], lon = longitudes[i], tz = "UTC")
    sun_info_yesterday <- getSunlightTimes(date = date(current_time - days(1)), lat = latitudes[i], lon = longitudes[i], tz = "UTC")
    
    # Define nighttime intervals
    night_intervals <- c(
      interval(start = pmax(sun_info_yesterday$sunset, start_of_period), end = sun_info_today$sunrise),
      interval(start = sun_info_today$sunset, end = pmin(end_of_period, sun_info_today$sunset + hours(12)))
    )
    
    # Generate hourly timestamps within the 24-hour window
    hourly_timestamps <- seq(from = start_of_period, to = end_of_period, by = "hour")
    
    # Determine which timestamps are in the nighttime
    nighttime_hours <- hourly_timestamps[sapply(hourly_timestamps, function(x) any(x %within% night_intervals))]
    
    # Store the nighttime hours
    nighttime_hours_all[[i]] <- round_date(nighttime_hours, unit = "hour")
  }
  
  return(nighttime_hours_all)
}


extract_avg_night_environmental_data <- function(timestamps, latitudes, longitudes, IDs, shift_hours, path) {
  require(terra)
  require(lubridate)
  require(suncalc)
  require(dplyr)
  
  files = list.files(path, pattern = "*europe-full.nc")
  
  # Check if the length of latitudes and longitudes are the same
  if (length(latitudes) != length(longitudes)) {
    stop("The length of latitudes and longitudes vectors must be the same.")
  }
  
  # Check if the length of timestamps matches the locations
  if (length(timestamps) != length(latitudes)) {
    stop("The length of timestamps must match the number of location points provided.")
  }
  
  # Convert timestamps to POSIXct
  new_timestamps <- as.POSIXct(timestamps + shift_hours * 3600, tz = "UTC")
  
  # Create a data frame to hold the extracted environmental data
  all_extracted_data <- data.frame(timestamp = NA, longitude = NA, latitude = NA, ID = NA)
  
  # Loop over each timestamp
  print("Entering main loop")
  i = 49
  for (i in 1:length(timestamps)) {
    print(paste("Processing timestamp:", i, "of", length(timestamps)))
    
    night_hours <- extract_nighttime_hours(new_timestamps[i], latitudes[i], longitudes[i])
    
    # Find the file that contains the date
    matching_file <- grep(format(new_timestamps[i], "%Y-%m-%d"), files, value = TRUE)
    if (length(matching_file) == 0) next # Skip if no matching file is found
    # print(paste("Matching file found:", matching_file))
    # Load the raster file
    env_raster <- rast(file.path(path, matching_file))
    # plot(env_raster[[1]])
    # Extract data for the specific time period
    coords <- cbind(longitudes[i], latitudes[i])
    idx <- which(time(env_raster) %in% night_hours[[1]])
    env_extract <- {}
    
    try({
      env_extract <- raster::extract(env_raster[[idx]],
                                     matrix(coords, ncol = 2),
                                     method = 'simple')
      
      # Check if data is extracted
      if (length(env_extract) == 0) {
        print(paste("No data extracted for timestamp:", new_timestamps[i]))
        next
      }
      
      # Rename the columns
      names(env_extract) <- gsub("_\\d+$", "", names(env_extract))
      # Reshaping the data to long format
      env_extract_rows <- combine_columns(env_extract)
      env_extract_rows$timestamp <- timestamps[i]
      env_extract_rows$nighttime <- night_hours[[1]] #timestamps[i]
      env_extract_rows$longitude <- longitudes[i]
      env_extract_rows$latitude <- latitudes[i]
      env_extract_rows$ID <- IDs[i]
      
      # Check the extracted data
      # print(paste("Extracted data for timestamp:", new_timestamps[i]))
      # print(head(env_extract_rows))
      
      # Combine the extracted data with all_extracted_data
      if (nrow(env_extract_rows) > 0) {
        all_extracted_data <- full_join(all_extracted_data, env_extract_rows)
      }
    })
  }
  # summary(all_extracted_data)
  # table(all_extracted_data$timestamp) |> length()
  # which(timestamps %in%  all_extracted_data$timestamp)
  # Calculate average values for night time data
  avg_night_data <- all_extracted_data |>
    dplyr::group_by(latitude, longitude, ID, timestamp) |>
    #summarise(n = n())
    dplyr::summarise(across(-c(tp, nighttime), \(x) mean(x, na.rm = TRUE)),
                     tp = sum(tp, na.rm = TRUE))
  names(avg_night_data)[c(5:ncol(avg_night_data))] <- paste0(names(avg_night_data)[c(5:ncol(avg_night_data))],
                                                             "_", shift_hours)
  summary(avg_night_data)
  # Return the combined extracted environmental data
  return(list("night_data" = all_extracted_data, "average_night_data" = avg_night_data))
}
