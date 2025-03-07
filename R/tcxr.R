library(XML)

#' Read and Parse a TCX File
#'
#' Parses a TCX file to extract key activity metrics such as speed, distance, time, altitude, power, cadence, heart rate, and activity type.
#'
#' @param file_path A character string specifying the path to the TCX file.
#' @return A list containing the computed activity metrics, including the activity type.
#' @importFrom XML xmlTreeParse xmlRoot getNodeSet xmlGetAttr xmlValue
#' @export
TCXRead <- function(file_path) {
  doc <- XML::xmlTreeParse(file_path, useInternalNodes = TRUE)
  root <- XML::xmlRoot(doc)

  activities <- XML::getNodeSet(root, "//ns:Activities/ns:Activity", namespaces = c(ns = "http://www.garmin.com/xmlschemas/TrainingCenterDatabase/v2"))

  activity_data <- lapply(activities, function(activity) {
    # Get the activity type (Sport) from the activity node
    activity_type <- XML::xmlGetAttr(activity, "Sport")

    laps <- XML::getNodeSet(activity, "ns:Lap", namespaces = c(ns = "http://www.garmin.com/xmlschemas/TrainingCenterDatabase/v2"))
    lap_data <- lapply(laps, parse_lap)

    # Return the lap data along with activity type
    return(list(activity_type = activity_type, lap_data = do.call(rbind, lap_data)))
  })

  # Combine all activity data
  all_activity_data <- lapply(activity_data, function(activity) {
    cbind(activity$lap_data, activity_type = activity$activity_type)
  })

  activity_df <- do.call(rbind, all_activity_data)

  # Aggregate activity metrics
  total_time_seconds <- sum(activity_df$total_time_seconds, na.rm = TRUE)
  total_distance_meters <- sum(activity_df$distance_meters, na.rm = TRUE)
  total_calories <- sum(activity_df$calories, na.rm = TRUE)
  max_altitude <- max(activity_df$max_altitude, na.rm = TRUE)
  total_ascent <- sum(activity_df$total_ascent, na.rm = TRUE)
  total_descent <- sum(activity_df$total_descent, na.rm = TRUE)

  # Speed calculations
  valid_speeds <- activity_df$max_speed[is.finite(activity_df$max_speed)]
  max_speed <- ifelse(length(valid_speeds) > 0, max(valid_speeds, na.rm = TRUE), NA)
  average_speed <- ifelse(total_time_seconds > 0, (total_distance_meters / total_time_seconds) * 3.6, NA)

  # Watts calculations
  valid_watts <- activity_df$max_watts[is.finite(activity_df$max_watts)]
  max_watts <- ifelse(length(valid_watts) > 0, max(valid_watts, na.rm = TRUE), NA)
  valid_avg_watts <- unlist(lapply(activity_data, function(x) x$average_watts))
  average_watts <- ifelse(length(valid_avg_watts) > 0, mean(valid_avg_watts, na.rm = TRUE), NA)

  # Cadence calculations
  valid_cadence <- activity_df$max_cadence[is.finite(activity_df$max_cadence)]
  max_cadence <- ifelse(length(valid_cadence) > 0, max(valid_cadence, na.rm = TRUE), NA)
  valid_avg_cadence <- activity_df$average_cadence[is.finite(activity_df$average_cadence)]
  average_cadence <- ifelse(length(valid_avg_cadence) > 0, mean(valid_avg_cadence, na.rm = TRUE), NA)

  # Heart Rate calculations
  valid_heart_rates <- activity_df$max_hr[is.finite(activity_df$max_hr)]
  max_hr <- ifelse(length(valid_heart_rates) > 0, max(valid_heart_rates, na.rm = TRUE), NA)
  valid_avg_hr <- activity_df$average_hr[is.finite(activity_df$average_hr)]
  average_hr <- ifelse(length(valid_avg_hr) > 0, mean(valid_avg_hr, na.rm = TRUE), NA)

  # Extract unique activity types
  activity_types <- unique(activity_df$activity_type)

  return(list(
    total_distance_meters = total_distance_meters,
    total_time_seconds = total_time_seconds,
    total_calories = total_calories,
    max_altitude = max_altitude,
    total_ascent = total_ascent,
    total_descent = total_descent,
    average_speed_kmh = average_speed,
    max_speed_kmh = max_speed,
    max_watts = max_watts,
    average_watts = average_watts,
    max_cadence = max_cadence,
    average_cadence = average_cadence,
    max_hr = max_hr,
    average_hr = average_hr,
    activity_types = activity_types
  ))
}


#' Parse a Lap from a TCX File
#'
#' Extracts data from a lap within a TCX file, including time, distance, altitude, speed, power, cadence, and heart rate.
#'
#' @param lap An XML node representing a lap in a TCX file.
#' @return A dataframe containing the lap metrics.
parse_lap <- function(lap) {
  trackpoints <- XML::getNodeSet(lap, "ns:Track/ns:Trackpoint", namespaces = c(ns = "http://www.garmin.com/xmlschemas/TrainingCenterDatabase/v2"))
  track_data <- lapply(trackpoints, parse_trackpoint)

  total_time_seconds <- as.numeric(XML::xmlValue(lap[["TotalTimeSeconds"]]))
  distance_meters <- as.numeric(XML::xmlValue(lap[["DistanceMeters"]]))
  calories <- as.numeric(XML::xmlValue(lap[["Calories"]]))

  altitude_values <- unlist(lapply(track_data, function(x) x$altitude_meters))
  altitude_diff <- diff(altitude_values, differences = 1)
  total_ascent <- ifelse(length(altitude_diff) > 0, sum(altitude_diff[altitude_diff > 0], na.rm = TRUE), NA)
  total_descent <- ifelse(length(altitude_diff) > 0, sum(-altitude_diff[altitude_diff < 0], na.rm = TRUE), NA)

  # Ensure max() does not return -Inf
  safe_max <- function(x) if (length(x) > 0 && any(!is.na(x))) max(x, na.rm = TRUE) else NA

  max_altitude <- safe_max(altitude_values)
  max_speed <- safe_max(unlist(lapply(track_data, function(x) x$speed_mps))) * 3.6
  max_watts <- safe_max(unlist(lapply(track_data, function(x) x$watts)))
  max_cadence <- safe_max(unlist(lapply(track_data, function(x) x$cadence)))
  average_cadence <- ifelse(length(track_data) > 0, mean(unlist(lapply(track_data, function(x) x$cadence)), na.rm = TRUE), NA)
  max_hr <- safe_max(unlist(lapply(track_data, function(x) x$heart_rate)))
  average_hr <- ifelse(length(track_data) > 0, mean(unlist(lapply(track_data, function(x) x$heart_rate)), na.rm = TRUE), NA)

  return(data.frame(
    total_time_seconds = total_time_seconds,
    distance_meters = distance_meters,
    calories = calories,
    max_altitude = max_altitude,
    total_ascent = total_ascent,
    total_descent = total_descent,
    max_speed = max_speed,
    max_watts = max_watts,
    max_cadence = max_cadence,
    average_cadence = average_cadence,
    max_hr = max_hr,
    average_hr = average_hr
  ))
}

#' Parse a Trackpoint from a TCX File
#'
#' Extracts data from a trackpoint, including altitude, distance, speed, power, cadence, and heart rate.
#'
#' @param trackpoint An XML node representing a trackpoint.
#' @return A list of parsed trackpoint metrics.
parse_trackpoint <- function(trackpoint) {
  altitude_meters <- tryCatch(as.numeric(XML::xmlValue(trackpoint[["AltitudeMeters"]])), error = function(e) NA, warning = function(w) NA)
  speed_mps <- tryCatch(as.numeric(XML::xmlValue(trackpoint[["Extensions"]][["TPX"]][["Speed"]])), error = function(e) NA, warning = function(w) NA)
  watts <- tryCatch(as.numeric(XML::xmlValue(trackpoint[["Extensions"]][["TPX"]][["Watts"]])), error = function(e) NA, warning = function(w) NA)
  cadence <- tryCatch(as.numeric(XML::xmlValue(trackpoint[["Cadence"]])), error = function(e) NA, warning = function(w) NA)
  heart_rate <- tryCatch(as.numeric(XML::xmlValue(trackpoint[["HeartRateBpm"]][["Value"]])), error = function(e) NA, warning = function(w) NA)

  return(list(
    altitude_meters = altitude_meters,
    speed_mps = speed_mps,
    watts = watts,
    cadence = cadence,
    heart_rate = heart_rate
  ))
}

