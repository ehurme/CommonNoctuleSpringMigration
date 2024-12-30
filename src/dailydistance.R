# Function: Regularize tracks to daily samples
dailydistance <- function(m){
  if(any(is(m) == "data.frame")){
    require(pacman)
    p_load(tidyverse, dplyr, lubridate, elevatr, raster, move, numbers)
    suppressWarnings({
      m |> mutate(date = date(timestamp), ID = tag_local_identifier) |>
        reframe(lat = location_lat[which.min(abs(time_to_noon))],
                lon = location_long[which.min(abs(time_to_noon))],
                diff_noon = time_to_noon[which.min(abs(time_to_noon))],
                time = timestamp[which.min(abs(time_to_noon))],
                vedba_min = min(tinyfox_total_vedba_mg, na.rm = TRUE),
                vedba_max = max(tinyfox_total_vedba_mg, na.rm = TRUE),
                vedba_noon = tinyfox_total_vedba_mg[which.min(abs(time_to_noon))],
                activity = tinyfox_activity_percent_last_24h[which.min(abs(time_to_noon))],
                radius = max(sigfox_computed_location_radius)/1000,
                accuracy = sigfox_link_quality[which.min(abs(time_to_noon))],
                min_temp = min(tinyfox_temperature_min_last_24h, na.rm = TRUE),
                max_temp = max(tinyfox_temperature_max_last_24h, na.rm = TRUE),
                capture_mass = first(animal_mass),
                fa = first(fa),
                tag_mass = first(tag_mass_total),
                is_capture_location = first(is_capture_point),
                day_night = first(day_night),
                .by = c(ID, burst_id)) -> m_day
    })
    m_day$vedba_min[which(is.infinite(m_day$vedba_min))] <- NA
    m_day$vedba_max[which(is.infinite(m_day$vedba_max))] <- NA
    m_day$min_temp[which(is.infinite(m_day$min_temp))] <- NA
    m_day$max_temp[which(is.infinite(m_day$max_temp))] <- NA
    summary(m_day)
    
    IDs <- unique(m_day$ID)
    m_day$distance <- NA
    m_day$diff_vedba <- NA
    m_day$daily_vedba <- NA
    m_day$diff_time <- NA
    m_day$time_running <- NA
    m_day$bearing <- NA
    #i = 82
    
    m_day <- m_day[order(m_day$ID, m_day$time),]
    i = 1
    for(i in 1:length(IDs)){
      idx <- which(m_day$ID == IDs[i])
      k = 1
      for(k in 1:(length(idx)-1)){
        try({
          suppressMessages({
            m_day$distance[idx[k]] <- raster::pointDistance(c(m_day$lon[idx[k+1]], m_day$lat[idx[k+1]]),
                                                            c(m_day$lon[idx[k]], m_day$lat[idx[k]]),
                                                            lonlat=TRUE)/1000  
          })
          m_day$diff_time[idx[k]] <- as.numeric(difftime(m_day$time[idx[k+1]],
                                                         m_day$time[idx[k]],
                                                         units = "days"))
        try(m_day$time_running[idx[k]] <- difftime(m_day$time[idx[k]], m_day$time[idx[1]], units = "days"))
        m_day$diff_vedba[idx[k]] <-  (as.numeric(m_day$vedba_noon[idx[k+1]] - m_day$vedba_noon[idx[k]]))
        m_day$daily_vedba[idx[k]] <-  m_day$diff_vedba[idx[k]]/(m_day$diff_time[idx[k]])
        try({m_day$bearing[idx[k]] <- calculate_bearing(lon1 = m_day$lon[idx[k]],
                                                        lat1 = m_day$lat[idx[k]],
                                                        lon2 = m_day$lon[idx[k+1]],
                                                        lat2 = m_day$lat[idx[k+1]])
        })
        })
      }
    }
    m_day <- m_day[order(m_day$ID),]
    m_day$daily_distance <- m_day$distance/m_day$diff_time
    return(m_day)
  }
}