library(XML)
library(dplyr)

#' Parse a TCX file and extract activity metrics
#'
#' This function reads a TCX file and extracts key activity metrics including total distance,
#' total time, calories burned, maximum altitude, and power values (watts).
#'
#' @param file_path A string specifying the path to the TCX file.
#' @return A list containing the total distance in meters, total time in seconds,
#' total calories burned, maximum altitude reached, maximum watts, and average watts.
#' @export
TCXRead <- function(file_path) {
  doc <- xmlTreeParse(file_path, useInternalNodes = TRUE)
  root <- xmlRoot(doc)

  # Extract activities
  activities <- getNodeSet(root, "//ns:Activities/ns:Activity", namespaces = c(ns = "http://www.garmin.com/xmlschemas/TrainingCenterDatabase/v2"))

  # Initialize values
  total_distance_meters <- 0
  total_time_seconds <- 0
  total_calories <- 0
  max_altitude <- 0
  max_watts <- NA
  average_watts <- NA

  # Parse activities
  activity_data <- lapply(activities, function(activity) {
    laps <- getNodeSet(activity, "ns:Lap", namespaces = c(ns = "http://www.garmin.com/xmlschemas/TrainingCenterDatabase/v2"))
    lap_data <- lapply(laps, parse_lap)

    total_time_seconds <- sum(sapply(lap_data, `[[`, "total_time_seconds"))
    total_distance_meters <- sum(sapply(lap_data, `[[`, "distance_meters"))
    total_calories <- sum(sapply(lap_data, `[[`, "calories"))
    max_altitude <- max(sapply(lap_data, `[[`, "max_altitude"), na.rm = TRUE)

    watts_values <- unlist(sapply(lap_data, `[[`, "watts"))
    max_watts <- ifelse(length(watts_values) > 0, max(watts_values, na.rm = TRUE), NA)
    average_watts <- ifelse(length(watts_values) > 0, mean(watts_values, na.rm = TRUE), NA)

    list(
      total_time_seconds = total_time_seconds,
      total_distance_meters = total_distance_meters,
      total_calories = total_calories,
      max_altitude = max_altitude,
      max_watts = max_watts,
      average_watts = average_watts,
      lap_data = lap_data
    )
  })

  total_time_seconds <- sum(sapply(activity_data, `[[`, "total_time_seconds"))
  total_distance_meters <- sum(sapply(activity_data, `[[`, "total_distance_meters"))
  total_calories <- sum(sapply(activity_data, `[[`, "total_calories"))
  max_altitude <- max(sapply(activity_data, `[[`, "max_altitude"), na.rm = TRUE)
  max_watts <- max(sapply(activity_data, `[[`, "max_watts"), na.rm = TRUE)
  average_watts <- mean(sapply(activity_data, `[[`, "average_watts"), na.rm = TRUE)

  return(list(
    total_distance_meters = total_distance_meters,
    total_time_seconds = total_time_seconds,
    total_calories = total_calories,
    max_altitude = max_altitude,
    max_watts = max_watts,
    average_watts = average_watts
  ))
}

#' Parse a lap from a TCX file
#'
#' This function extracts lap details including time, distance, calories, maximum altitude, and watts.
#'
#' @param lap An XML node representing a lap.
#' @return A list containing the lap time, distance, calories burned, maximum altitude, and power values.
parse_lap <- function(lap) {
  trackpoints <- getNodeSet(lap, "ns:Track/ns:Trackpoint", namespaces = c(ns = "http://www.garmin.com/xmlschemas/TrainingCenterDatabase/v2"))
  track_data <- lapply(trackpoints, parse_trackpoint)

  total_time_seconds <- as.numeric(xmlValue(lap[["TotalTimeSeconds"]]))
  distance_meters <- as.numeric(xmlValue(lap[["DistanceMeters"]]))
  calories <- as.numeric(xmlValue(lap[["Calories"]]))
  max_altitude <- max(sapply(track_data, `[[`, "altitude_meters"), na.rm = TRUE)
  watts <- sapply(track_data, `[[`, "watts")

  return(list(
    total_time_seconds = total_time_seconds,
    distance_meters = distance_meters,
    calories = calories,
    max_altitude = max_altitude,
    watts = watts,
    track_data = track_data
  ))
}

#' Parse a trackpoint from a TCX file
#'
#' This function extracts trackpoint details such as altitude, distance, heart rate, and power values.
#'
#' @param trackpoint An XML node representing a trackpoint.
#' @return A list containing the altitude, distance, heart rate, and power output at the trackpoint.
parse_trackpoint <- function(trackpoint) {
  altitude_meters <- as.numeric(xmlValue(trackpoint[["AltitudeMeters"]]))
  distance_meters <- as.numeric(xmlValue(trackpoint[["DistanceMeters"]]))
  heart_rate <- as.numeric(xmlValue(trackpoint[["HeartRateBpm"]][["Value"]]))
  watts <- as.numeric(xmlValue(trackpoint[["Extensions"]][["TPX"]][["Watts"]]))

  return(list(
    altitude_meters = altitude_meters,
    distance_meters = distance_meters,
    heart_rate = heart_rate,
    watts = watts
  ))
}

