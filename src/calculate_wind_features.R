#######################################
# Function: Calculate Wind Features
# Purpose: Calculate wind-related features such as wind speed, direction, wind support,
#          crosswind, and airspeed for specified time offsets.
#######################################

# Function to calculate wind features
calculate_wind_features <- function(data, u_col_base, v_col_base,
                                    windsp_col_base, winddir_col_base,
                                    distance_col, time_diff_col, bearing_col,
                                    hours) {
  require(stringr)
  
  # Extract elevation level from column names (e.g., "100" from "u100")
  elevation <- str_extract(v_col_base, "[0-9]+$")
  
  # Loop through the specified hours to calculate wind features
  for (h in hours) {
    # Define dynamic column names based on the hour offset
    u_col_name <- paste0(u_col_base, "_", h)
    v_col_name <- paste0(v_col_base, "_", h)
    windsp_col_name <- paste0(windsp_col_base, "_", h)
    winddir_col_name <- paste0(winddir_col_base, "_", h)
    ws_col_name <- paste0("ws", elevation, "_", h)
    cw_col_name <- paste0("cw", elevation, "_", h)
    airspeed_col_name <- paste0("airspeed", elevation, "_", h)
    
    # Calculate wind speed and direction
    data[[windsp_col_name]] <- with(data, sqrt(get(u_col_name)^2 + get(v_col_name)^2))
    data[[winddir_col_name]] <- with(data, wind_dir(u_ms = get(u_col_name), v_ms = get(v_col_name)))
    
    # Calculate wind support and crosswind
    data[[ws_col_name]] <- with(data, wind_support(u = get(u_col_name), v = get(v_col_name), heading = get(bearing_col)))
    data[[cw_col_name]] <- with(data, cross_wind(u = get(u_col_name), v = get(v_col_name), heading = get(bearing_col)))
    
    # Calculate airspeed
    ground_speed <- with(data, get(distance_col) / (get(time_diff_col) * 60))  # Convert minutes to seconds
    data[[airspeed_col_name]] <- with(data, airspeed(ground_speed = ground_speed, ws = get(ws_col_name), cw = get(cw_col_name)))
  }
  
  # Return the updated data frame
  return(data)
}

#######################################
# Example Usage
# Assuming 'n_env' is your dataframe, 'u100' and 'v100' are base names for wind components,
# 'distance' is the total distance traveled, 'diff_time' is the time difference in minutes,
# 'bearing' is the heading angle, and 'hours' is a vector of time offsets.
#
# n_env <- calculate_wind_features(n_env, "u100", "v100", "windsp100", "winddir100",
#                                  "distance", "diff_time", "bearing", hours = seq(-120, 120, by = 24))
#######################################
# End of Function
#######################################
