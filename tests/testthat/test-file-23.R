library(tcxr)
library(XML)
library(testthat)

# TCX test files
# Reference: I. Jr. Fister, S. Rauter, D. Fister, I. Fister. A collection of sport activity datasets with an emphasis on powermeter data. Technical report, University of Maribor, 2017.
# More info: https://github.com/firefly-cpp/tcx-test-files

sample_tcx_file <- "test_files/23.tcx"

test_that("TCXRead correctly parses a valid TCX test file", {
  result <- TCXRead(sample_tcx_file)

  expect_type(result, "list")
  expect_true("summary" %in% names(result))
  expect_true("raw_data" %in% names(result))

  expect_type(result$summary, "list")
  expect_s3_class(result$raw_data, "data.frame")

  expect_type(result$summary$total_distance_meters, "double")
  expect_type(result$summary$total_time_seconds, "double")
  expect_type(result$summary$total_calories, "double")
  expect_type(result$summary$max_altitude, "double")
  expect_type(result$summary$total_ascent, "double")
  expect_type(result$summary$total_descent, "double")
  expect_type(result$summary$average_speed_kmh, "double")
  expect_type(result$summary$max_speed_kmh, "double")
  expect_type(result$summary$max_watts, "double")
  expect_type(result$summary$average_watts, "double")
  expect_type(result$summary$max_cadence, "double")
  expect_type(result$summary$average_cadence, "double")
  expect_type(result$summary$max_hr, "double")
  expect_type(result$summary$average_hr, "double")

  expect_true(nrow(result$raw_data) > 0)

  expected_raw_cols <- c(
    "activity_id", "lap_id", "trackpoint_id", "lap_start_time",
    "time", "time_parsed", "distance_meters", "altitude_meters",
    "speed_mps", "speed_kmh", "watts", "cadence", "heart_rate",
    "activity_type"
  )

  expect_true(all(expected_raw_cols %in% names(result$raw_data)))
})

test_that("TCXRead returns expected values for a known TCX test file", {
  result <- TCXRead(sample_tcx_file)

  expected_distance <- 134500.8
  expected_time <- 15496
  expected_calories <- 3245
  expected_max_altitude <- 666.2
  expected_total_ascent <- 1528
  expected_total_descent <- 1527.2
  expected_avg_speed <- 31.24695
  expected_max_speed <- 76.014
  expected_max_watts <- 587
  expected_max_cadence <- 114
  expected_avg_cadence <- 79.5 # TODO:: Test it with tcxreader in Python
  expected_max_hr <- 162
  expected_avg_hr <- 139.6313

  expect_equal(result$summary$total_distance_meters, expected_distance, tolerance = 0.01)
  expect_equal(result$summary$total_time_seconds, expected_time, tolerance = 0.01)
  expect_equal(result$summary$total_calories, expected_calories, tolerance = 0.01)
  expect_equal(result$summary$max_altitude, expected_max_altitude, tolerance = 0.01)
  expect_equal(result$summary$total_ascent, expected_total_ascent, tolerance = 0.01)
  expect_equal(result$summary$total_descent, expected_total_descent, tolerance = 0.01)
  expect_equal(result$summary$average_speed_kmh, expected_avg_speed, tolerance = 0.01)
  expect_equal(result$summary$max_speed_kmh, expected_max_speed, tolerance = 0.01)
  expect_equal(result$summary$max_watts, expected_max_watts, tolerance = 0.01)
  expect_equal(result$summary$max_cadence, expected_max_cadence, tolerance = 0.01)
  expect_equal(result$summary$average_cadence, expected_avg_cadence, tolerance = 0.01)
  expect_equal(result$summary$max_hr, expected_max_hr, tolerance = 0.01)
  expect_equal(result$summary$average_hr, expected_avg_hr, tolerance = 0.01)
})

test_that("TCXRead returns raw trackpoint dataframe", {
  result <- TCXRead(sample_tcx_file)

  expect_s3_class(result$raw_data, "data.frame")
  expect_true(nrow(result$raw_data) > 0)

  expect_true(all(is.na(result$raw_data$speed_kmh) |
    abs(result$raw_data$speed_kmh - result$raw_data$speed_mps * 3.6) < 1e-8))
})


