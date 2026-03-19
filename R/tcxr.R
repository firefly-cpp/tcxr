library(XML)

#' Read and Parse a TCX File
#'
#' Parses a TCX file and returns both:
#' 1. aggregated activity metrics
#' 2. raw trackpoint data as a dataframe
#'
#' @param file_path A character string specifying the path to the TCX file.
#' @return A list with:
#'   - summary: named list of aggregated metrics
#'   - raw_data: dataframe of raw TCX trackpoints
#' @importFrom XML xmlTreeParse xmlRoot getNodeSet xmlGetAttr xmlValue
#' @export
#' @examples
#' # Example usage
#' temp_tcx_file <- tempfile(fileext = ".tcx")
#' cat('<?xml version="1.0" encoding="UTF-8"?>
#' <TrainingCenterDatabase xmlns="http://www.garmin.com/xmlschemas/TrainingCenterDatabase/v2">
#'   <Activities>
#'     <Activity Sport="Running">
#'       <Lap StartTime="2024-01-01T10:00:00Z">
#'         <TotalTimeSeconds>1800</TotalTimeSeconds>
#'         <DistanceMeters>5000</DistanceMeters>
#'         <Calories>400</Calories>
#'         <Track>
#'           <Trackpoint>
#'             <Time>2024-01-01T10:00:00Z</Time>
#'             <DistanceMeters>0</DistanceMeters>
#'             <AltitudeMeters>50</AltitudeMeters>
#'             <Extensions>
#'               <TPX>
#'                 <Speed>2.78</Speed>
#'                 <Watts>200</Watts>
#'               </TPX>
#'             </Extensions>
#'             <Cadence>90</Cadence>
#'             <HeartRateBpm>
#'               <Value>150</Value>
#'             </HeartRateBpm>
#'           </Trackpoint>
#'         </Track>
#'       </Lap>
#'     </Activity>
#'   </Activities>
#' </TrainingCenterDatabase>', file = temp_tcx_file)
#'
#' tcx_data <- TCXRead(temp_tcx_file)
#' tcx_data$summary
#' head(tcx_data$raw_data)
#'
#' unlink(temp_tcx_file)
TCXRead <- function(file_path) {
  ns <- c(ns = "http://www.garmin.com/xmlschemas/TrainingCenterDatabase/v2")

  doc <- XML::xmlTreeParse(file_path, useInternalNodes = TRUE)
  root <- XML::xmlRoot(doc)

  activities <- XML::getNodeSet(root, "//ns:Activities/ns:Activity", namespaces = ns)

  if (length(activities) == 0) {
    return(list(
      summary = list(
        total_distance_meters = NA_real_,
        total_time_seconds = NA_real_,
        total_calories = NA_real_,
        max_altitude = NA_real_,
        total_ascent = NA_real_,
        total_descent = NA_real_,
        average_speed_kmh = NA_real_,
        max_speed_kmh = NA_real_,
        max_watts = NA_real_,
        average_watts = NA_real_,
        max_cadence = NA_real_,
        average_cadence = NA_real_,
        max_hr = NA_real_,
        average_hr = NA_real_,
        activity_types = character(0)
      ),
      raw_data = data.frame()
    ))
  }

  activity_data <- lapply(seq_along(activities), function(activity_index) {
    activity <- activities[[activity_index]]
    activity_type <- XML::xmlGetAttr(activity, "Sport")

    laps <- XML::getNodeSet(activity, "ns:Lap", namespaces = ns)

    if (length(laps) == 0) {
      return(list(
        summary = data.frame(),
        raw = data.frame(),
        activity_type = activity_type
      ))
    }

    lap_data <- lapply(seq_along(laps), function(lap_index) {
      parse_lap(
        lap = laps[[lap_index]],
        activity_id = activity_index,
        lap_id = lap_index
      )
    })

    summary_df <- do.call(rbind, lapply(lap_data, function(x) x$summary))
    raw_df <- do.call(rbind, lapply(lap_data, function(x) x$raw))

    if (!is.null(raw_df) && nrow(raw_df) > 0) {
      raw_df$activity_type <- activity_type
      raw_df$activity_id <- activity_index
    }

    list(
      summary = summary_df,
      raw = raw_df,
      activity_type = activity_type
    )
  })

  activity_summaries <- lapply(activity_data, function(x) x$summary)
  activity_summaries <- activity_summaries[lengths(activity_summaries) > 0]
  activity_df <- if (length(activity_summaries) > 0) do.call(rbind, activity_summaries) else data.frame()

  raw_dfs <- lapply(activity_data, function(x) x$raw)
  raw_dfs <- raw_dfs[lengths(raw_dfs) > 0]
  raw_df <- if (length(raw_dfs) > 0) do.call(rbind, raw_dfs) else data.frame()

  safe_max <- function(x) {
    if (length(x) > 0 && any(!is.na(x) & is.finite(x))) {
      max(x[is.finite(x)], na.rm = TRUE)
    } else {
      NA_real_
    }
  }

  safe_mean <- function(x) {
    x <- x[is.finite(x)]
    if (length(x) > 0) mean(x, na.rm = TRUE) else NA_real_
  }

  safe_sum <- function(x) {
    if (length(x) > 0 && any(!is.na(x))) sum(x, na.rm = TRUE) else NA_real_
  }

  if (nrow(activity_df) == 0) {
    return(list(
      summary = list(
        total_distance_meters = NA_real_,
        total_time_seconds = NA_real_,
        total_calories = NA_real_,
        max_altitude = NA_real_,
        total_ascent = NA_real_,
        total_descent = NA_real_,
        average_speed_kmh = NA_real_,
        max_speed_kmh = NA_real_,
        max_watts = NA_real_,
        average_watts = NA_real_,
        max_cadence = NA_real_,
        average_cadence = NA_real_,
        max_hr = NA_real_,
        average_hr = NA_real_,
        activity_types = unique(unlist(lapply(activity_data, function(x) x$activity_type)))
      ),
      raw_data = raw_df
    ))
  }

  total_time_seconds <- safe_sum(activity_df$total_time_seconds)
  total_distance_meters <- safe_sum(activity_df$distance_meters)
  total_calories <- safe_sum(activity_df$calories)
  max_altitude <- safe_max(activity_df$max_altitude)
  total_ascent <- safe_sum(activity_df$total_ascent)
  total_descent <- safe_sum(activity_df$total_descent)

  max_speed <- safe_max(activity_df$max_speed_kmh)
  average_speed <- if (!is.na(total_time_seconds) && total_time_seconds > 0) {
    (total_distance_meters / total_time_seconds) * 3.6
  } else {
    NA_real_
  }

  max_watts <- safe_max(activity_df$max_watts)
  average_watts <- safe_mean(raw_df$watts)

  max_cadence <- safe_max(activity_df$max_cadence)
  average_cadence <- safe_mean(raw_df$cadence)

  max_hr <- safe_max(activity_df$max_hr)
  average_hr <- safe_mean(raw_df$heart_rate)

  activity_types <- unique(unlist(lapply(activity_data, function(x) x$activity_type)))

  list(
    summary = list(
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
    ),
    raw_data = raw_df
  )
}

#' Parse a Lap from a TCX File
#'
#' Extracts summary data from a lap and also returns raw trackpoint data.
#'
#' @param lap An XML node representing a lap in a TCX file.
#' @param activity_id Integer activity index.
#' @param lap_id Integer lap index within activity.
#' @return A list with:
#'   - summary: dataframe containing lap summary
#'   - raw: dataframe of lap trackpoints
parse_lap <- function(lap, activity_id = NA_integer_, lap_id = NA_integer_) {
  ns <- c(ns = "http://www.garmin.com/xmlschemas/TrainingCenterDatabase/v2")

  trackpoints <- XML::getNodeSet(lap, "ns:Track/ns:Trackpoint", namespaces = ns)

  raw_list <- lapply(seq_along(trackpoints), function(trackpoint_id) {
    parse_trackpoint(
      trackpoint = trackpoints[[trackpoint_id]],
      activity_id = activity_id,
      lap_id = lap_id,
      trackpoint_id = trackpoint_id
    )
  })

  raw_df <- if (length(raw_list) > 0) do.call(rbind, raw_list) else data.frame(
    activity_id = numeric(0),
    lap_id = numeric(0),
    trackpoint_id = numeric(0),
    lap_start_time = character(0),
    time = character(0),
    time_parsed = as.POSIXct(character(0), tz = "UTC"),
    distance_meters = numeric(0),
    altitude_meters = numeric(0),
    speed_mps = numeric(0),
    speed_kmh = numeric(0),
    watts = numeric(0),
    cadence = numeric(0),
    heart_rate = numeric(0)
  )

  get_child_value <- function(node, child_name) {
    tryCatch(as.numeric(XML::xmlValue(node[[child_name]])),
             error = function(e) NA_real_,
             warning = function(w) NA_real_)
  }

  total_time_seconds <- get_child_value(lap, "TotalTimeSeconds")
  distance_meters <- get_child_value(lap, "DistanceMeters")
  calories <- get_child_value(lap, "Calories")
  lap_start_time <- tryCatch(XML::xmlGetAttr(lap, "StartTime"),
                             error = function(e) NA_character_)

  safe_max <- function(x) {
    if (length(x) > 0 && any(!is.na(x) & is.finite(x))) {
      max(x[is.finite(x)], na.rm = TRUE)
    } else {
      NA_real_
    }
  }

  safe_mean <- function(x) {
    x <- x[is.finite(x)]
    if (length(x) > 0) mean(x, na.rm = TRUE) else NA_real_
  }

  altitude_values <- raw_df$altitude_meters
  altitude_values <- altitude_values[!is.na(altitude_values)]

  altitude_diff <- if (length(altitude_values) > 1) diff(altitude_values) else numeric(0)

  total_ascent <- if (length(altitude_diff) > 0) {
    sum(altitude_diff[altitude_diff > 0], na.rm = TRUE)
  } else {
    NA_real_
  }

  total_descent <- if (length(altitude_diff) > 0) {
    sum(-altitude_diff[altitude_diff < 0], na.rm = TRUE)
  } else {
    NA_real_
  }

  summary_df <- data.frame(
    activity_id = activity_id,
    lap_id = lap_id,
    lap_start_time = lap_start_time,
    total_time_seconds = total_time_seconds,
    distance_meters = distance_meters,
    calories = calories,
    max_altitude = safe_max(raw_df$altitude_meters),
    total_ascent = total_ascent,
    total_descent = total_descent,
    max_speed_kmh = safe_max(raw_df$speed_kmh),
    max_watts = safe_max(raw_df$watts),
    average_watts = safe_mean(raw_df$watts),
    max_cadence = safe_max(raw_df$cadence),
    average_cadence = safe_mean(raw_df$cadence),
    max_hr = safe_max(raw_df$heart_rate),
    average_hr = safe_mean(raw_df$heart_rate),
    stringsAsFactors = FALSE
  )

  list(
    summary = summary_df,
    raw = raw_df
  )
}

#' Parse a Trackpoint from a TCX File
#'
#' Extracts raw trackpoint fields into one dataframe row.
#'
#' @param trackpoint An XML node representing a trackpoint.
#' @param activity_id Integer activity index.
#' @param lap_id Integer lap index.
#' @param trackpoint_id Integer trackpoint index.
#' @return A one-row dataframe of parsed trackpoint metrics.
parse_trackpoint <- function(trackpoint,
                             activity_id = NA_integer_,
                             lap_id = NA_integer_,
                             trackpoint_id = NA_integer_) {
  get_numeric_value <- function(expr) {
    tryCatch(as.numeric(expr),
             error = function(e) NA_real_,
             warning = function(w) NA_real_)
  }

  get_text_value <- function(expr) {
    tryCatch(as.character(expr),
             error = function(e) NA_character_,
             warning = function(w) NA_character_)
  }

  altitude_meters <- tryCatch(
    get_numeric_value(XML::xmlValue(trackpoint[["AltitudeMeters"]])),
    error = function(e) NA_real_,
    warning = function(w) NA_real_
  )

  distance_meters <- tryCatch(
    get_numeric_value(XML::xmlValue(trackpoint[["DistanceMeters"]])),
    error = function(e) NA_real_,
    warning = function(w) NA_real_
  )

  cadence <- tryCatch(
    get_numeric_value(XML::xmlValue(trackpoint[["Cadence"]])),
    error = function(e) NA_real_,
    warning = function(w) NA_real_
  )

  heart_rate <- tryCatch(
    get_numeric_value(XML::xmlValue(trackpoint[["HeartRateBpm"]][["Value"]])),
    error = function(e) NA_real_,
    warning = function(w) NA_real_
  )

  speed_mps <- tryCatch(
    get_numeric_value(XML::xmlValue(trackpoint[["Extensions"]][["TPX"]][["Speed"]])),
    error = function(e) NA_real_,
    warning = function(w) NA_real_
  )

  watts <- tryCatch(
    get_numeric_value(XML::xmlValue(trackpoint[["Extensions"]][["TPX"]][["Watts"]])),
    error = function(e) NA_real_,
    warning = function(w) NA_real_
  )

  time <- tryCatch(
    get_text_value(XML::xmlValue(trackpoint[["Time"]])),
    error = function(e) NA_character_,
    warning = function(w) NA_character_
  )

  time_parsed <- tryCatch(
    as.POSIXct(time, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"),
    error = function(e) NA
  )

  lap_start_time <- NA_character_

  data.frame(
    activity_id = activity_id,
    lap_id = lap_id,
    trackpoint_id = trackpoint_id,
    lap_start_time = lap_start_time,
    time = time,
    time_parsed = time_parsed,
    distance_meters = distance_meters,
    altitude_meters = altitude_meters,
    speed_mps = speed_mps,
    speed_kmh = ifelse(is.na(speed_mps), NA_real_, speed_mps * 3.6),
    watts = watts,
    cadence = cadence,
    heart_rate = heart_rate,
    stringsAsFactors = FALSE
  )
}
