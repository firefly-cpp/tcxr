library(tcxr)
library(XML)
library(testthat)

# TCX test files
# Reference: I. Jr. Fister, S. Rauter, D. Fister, I. Fister. A collection of sport activity datasets with an emphasis on powermeter data. Technical report, University of Maribor, 2017.
# More info: https://github.com/firefly-cpp/tcx-test-files

sample_tcx_file <- "test_files/23.tcx"

test_that("TCXRead correctly parses a valid TCX test file", {
  result <- TCXRead(sample_tcx_file)

  # Check that the result is a list
  expect_type(result, "list")

  # Check values are numeric
  expect_type(result$total_distance_meters, "double")
  expect_type(result$total_time_seconds, "double")
  expect_type(result$total_calories, "double")
  expect_type(result$max_altitude, "double")
  expect_type(result$total_ascent, "double")
  expect_type(result$total_descent, "double")
  expect_type(result$average_speed_kmh, "double")
  expect_type(result$max_speed_kmh, "double")
  expect_type(result$max_watts, "double")
  expect_type(result$max_cadence, "double")
  expect_type(result$average_cadence, "double")
  expect_type(result$max_hr, "double")
  expect_type(result$average_hr, "double")
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
  expected_avg_cadence <- 75.36506
  expected_max_hr <- 162
  expected_avg_hr <- 139.6313

  expect_equal(result$total_distance_meters, expected_distance, tolerance = 0.01)
  expect_equal(result$total_time_seconds, expected_time, tolerance = 0.01)
  expect_equal(result$total_calories, expected_calories, tolerance = 0.01)
  expect_equal(result$max_altitude, expected_max_altitude, tolerance = 0.01)
  expect_equal(result$total_ascent, expected_total_ascent, tolerance = 0.01)
  expect_equal(result$total_descent, expected_total_descent, tolerance = 0.01)
  expect_equal(result$average_speed_kmh, expected_avg_speed, tolerance = 0.01)
  expect_equal(result$max_speed_kmh, expected_max_speed, tolerance = 0.01)
  expect_equal(result$max_watts, expected_max_watts, tolerance = 0.01)
  expect_equal(result$max_cadence, expected_max_cadence, tolerance = 0.01)
  expect_equal(result$average_cadence, expected_avg_cadence, tolerance = 0.01)
  expect_equal(result$max_hr, expected_max_hr, tolerance = 0.01)
  expect_equal(result$average_hr, expected_avg_hr, tolerance = 0.01)
})
