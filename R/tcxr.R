library(XML)

#' Parse a TCX file and extract activity metrics
TCXRead <- function(file_path) {
  doc <- xmlTreeParse(file_path, useInternalNodes = TRUE)
  root <- xmlRoot(doc)

  activities <- getNodeSet(root, "//ns:Activities/ns:Activity", namespaces = c(ns = "http://www.garmin.com/xmlschemas/TrainingCenterDatabase/v2"))

  activity_data <- lapply(activities, function(activity) {
    laps <- getNodeSet(activity, "ns:Lap", namespaces = c(ns = "http://www.garmin.com/xmlschemas/TrainingCenterDatabase/v2"))
    lap_data <- lapply(laps, parse_lap)
    return(do.call(rbind, lap_data))
  })

  activity_df <- do.call(rbind, activity_data)

  total_time_seconds <- sum(activity_df$total_time_seconds, na.rm = TRUE)
  total_distance_meters <- sum(activity_df$distance_meters, na.rm = TRUE)
  total_calories <- sum(activity_df$calories, na.rm = TRUE)
  max_altitude <- max(activity_df$max_altitude, na.rm = TRUE)
  total_ascent <- sum(activity_df$total_ascent, na.rm = TRUE)
  total_descent <- sum(activity_df$total_descent, na.rm = TRUE)

  valid_speeds <- activity_df$max_speed[!is.infinite(activity_df$max_speed) & !is.na(activity_df$max_speed)]
  max_speed <- ifelse(length(valid_speeds) > 0, max(valid_speeds, na.rm = TRUE), NA)
  average_speed <- ifelse(total_time_seconds > 0, total_distance_meters / total_time_seconds, NA)

  valid_moving_speeds <- activity_df$moving_average_speed[!is.na(activity_df$moving_average_speed)]
  moving_average_speed <- ifelse(length(valid_moving_speeds) > 0, mean(valid_moving_speeds, na.rm = TRUE), NA)

  max_watts <- max(activity_df$max_watts, na.rm = TRUE)
  average_watts <- mean(activity_df$average_watts, na.rm = TRUE)
  moving_average_watts <- mean(activity_df$moving_average_watts, na.rm = TRUE)

  max_cadence <- max(activity_df$max_cadence, na.rm = TRUE)
  average_cadence <- mean(activity_df$average_cadence, na.rm = TRUE)
  moving_average_cadence <- mean(activity_df$moving_average_cadence, na.rm = TRUE)

  max_heart_rate <- max(activity_df$max_heart_rate, na.rm = TRUE)
  average_heart_rate <- mean(activity_df$average_heart_rate, na.rm = TRUE)

  return(list(
    total_distance_meters = total_distance_meters,
    total_time_seconds = total_time_seconds,
    total_calories = total_calories,
    max_altitude = max_altitude,
    total_ascent = total_ascent,
    total_descent = total_descent,
    average_speed = average_speed,
    moving_average_speed = moving_average_speed,
    max_speed = max_speed,
    max_watts = max_watts,
    average_watts = average_watts,
    moving_average_watts = moving_average_watts,
    max_cadence = max_cadence,
    average_cadence = average_cadence,
    moving_average_cadence = moving_average_cadence,
    max_heart_rate = max_heart_rate,
    average_heart_rate = average_heart_rate
  ))
}

#' Parse a lap from a TCX file
parse_lap <- function(lap) {
  trackpoints <- getNodeSet(lap, "ns:Track/ns:Trackpoint", namespaces = c(ns = "http://www.garmin.com/xmlschemas/TrainingCenterDatabase/v2"))
  track_data <- lapply(trackpoints, parse_trackpoint)

  total_time_seconds <- as.numeric(xmlValue(lap[["TotalTimeSeconds"]]))
  distance_meters <- as.numeric(xmlValue(lap[["DistanceMeters"]]))
  calories <- as.numeric(xmlValue(lap[["Calories"]]))

  altitude_values <- as.numeric(unlist(sapply(track_data, `[[`, "altitude_meters")))
  if (length(altitude_values) > 1) {
    altitude_diff <- diff(altitude_values)
    total_ascent <- sum(altitude_diff[altitude_diff > 0], na.rm = TRUE)
    total_descent <- sum(-altitude_diff[altitude_diff < 0], na.rm = TRUE)
  } else {
    total_ascent <- NA
    total_descent <- NA
  }
  max_altitude <- max(altitude_values, na.rm = TRUE)

  speed_values <- as.numeric(unlist(sapply(track_data, `[[`, "speed_mps")))
  if (length(speed_values) > 0) {
    max_speed <- max(speed_values, na.rm = TRUE)
    moving_speed_values <- speed_values[speed_values > 0]
    moving_average_speed <- ifelse(length(moving_speed_values) > 0, mean(moving_speed_values, na.rm = TRUE), NA)
  } else {
    max_speed <- NA
    moving_average_speed <- NA
  }

  watts_values <- as.numeric(unlist(sapply(track_data, `[[`, "watts")))
  max_watts <- max(watts_values, na.rm = TRUE)
  average_watts <- mean(watts_values, na.rm = TRUE)
  moving_average_watts <- mean(watts_values[watts_values > 0], na.rm = TRUE)

  cadence_values <- as.numeric(unlist(sapply(track_data, `[[`, "cadence")))
  max_cadence <- max(cadence_values, na.rm = TRUE)
  average_cadence <- mean(cadence_values, na.rm = TRUE)
  moving_average_cadence <- mean(cadence_values[speed_values > 0], na.rm = TRUE)

  heart_rate_values <- as.numeric(unlist(sapply(track_data, `[[`, "heart_rate")))
  max_heart_rate <- max(heart_rate_values, na.rm = TRUE)
  average_heart_rate <- mean(heart_rate_values, na.rm = TRUE)

  return(data.frame(
    total_time_seconds = total_time_seconds,
    distance_meters = distance_meters,
    calories = calories,
    max_altitude = max_altitude,
    total_ascent = total_ascent,
    total_descent = total_descent,
    max_speed = max_speed,
    moving_average_speed = moving_average_speed,
    max_watts = max_watts,
    average_watts = average_watts,
    moving_average_watts = moving_average_watts,
    max_cadence = max_cadence,
    average_cadence = average_cadence,
    moving_average_cadence = moving_average_cadence,
    max_heart_rate = max_heart_rate,
    average_heart_rate = average_heart_rate
  ))
}

#' Parse a trackpoint from a TCX file
parse_trackpoint <- function(trackpoint) {
  altitude_meters <- tryCatch(as.numeric(xmlValue(trackpoint[["AltitudeMeters"]])), error = function(e) NA)
  distance_meters <- tryCatch(as.numeric(xmlValue(trackpoint[["DistanceMeters"]])), error = function(e) NA)
  heart_rate <- tryCatch(as.numeric(xmlValue(trackpoint[["HeartRateBpm"]][["Value"]])), error = function(e) NA)
  watts <- tryCatch(as.numeric(xmlValue(trackpoint[["Extensions"]][["TPX"]][["Watts"]])), error = function(e) NA)
  cadence <- tryCatch(as.numeric(xmlValue(trackpoint[["Cadence"]])), error = function(e) NA)

  speed_mps <- ifelse(length(distance_meters) > 1, c(0, diff(distance_meters)), NA)

  return(list(
    altitude_meters = altitude_meters,
    distance_meters = distance_meters,
    heart_rate = heart_rate,
    watts = watts,
    cadence = cadence,
    speed_mps = speed_mps
  ))
}
