library(tcxr)
library(XML)
library(testthat)

# TCX test files
# Reference: I. Jr. Fister, S. Rauter, D. Fister, I. Fister. A collection of sport activity datasets with an emphasis on powermeter data. Technical report, University of Maribor, 2017.
# More info: https://github.com/firefly-cpp/tcx-test-files

sample_tcx_file <- testthat::test_path("test_files", "cross-country-skiing_activity_1.tcx")

test_that("TCXRead returns expected values for a cross-country skiing file", {
  result <- TCXRead(sample_tcx_file)

  expected_distance <- 5692.01
  expected_time <- 2401.26
  expected_calories <- 532
  expected_max_altitude <- 2337.6
  expected_total_ascent <- 1118
  expected_total_descent <- 117.1997
  expected_max_speed <- 22.1688
  expected_max_hr <- 172
  expected_avg_hr <- 141 # TODO:: Compare it with the tcxreader in python

  expect_type(result, "list")
  expect_true("summary" %in% names(result))
  expect_true("raw_data" %in% names(result))

  expect_equal(result$summary$total_distance_meters, expected_distance, tolerance = 0.01)
  expect_equal(result$summary$total_time_seconds, expected_time, tolerance = 0.01)
  expect_equal(result$summary$total_calories, expected_calories, tolerance = 0.01)
  expect_equal(result$summary$max_altitude, expected_max_altitude, tolerance = 0.01)
  expect_equal(result$summary$total_ascent, expected_total_ascent, tolerance = 0.01)
  expect_equal(result$summary$total_descent, expected_total_descent, tolerance = 0.01)
  expect_equal(result$summary$max_speed_kmh, expected_max_speed, tolerance = 0.01)

  expect_true(is.na(result$summary$max_watts))
  expect_true(is.na(result$summary$max_cadence))
  expect_true(is.na(result$summary$average_cadence))

  expect_equal(result$summary$max_hr, expected_max_hr, tolerance = 0.01)
  expect_equal(result$summary$average_hr, expected_avg_hr, tolerance = 0.01)

  expect_s3_class(result$raw_data, "data.frame")
  expect_true(nrow(result$raw_data) > 0)
})
