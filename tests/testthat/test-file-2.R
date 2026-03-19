library(tcxr)
library(XML)
library(testthat)

# TCX test files
# Reference: I. Jr. Fister, S. Rauter, D. Fister, I. Fister. A collection of sport activity datasets with an emphasis on powermeter data. Technical report, University of Maribor, 2017.
# More info: https://github.com/firefly-cpp/tcx-test-files

sample_tcx_file <- testthat::test_path("test_files", "2.tcx")

test_that("TCXRead returns expected values for 2.tcx file", {
  result <- TCXRead(sample_tcx_file)

  expected_distance <- 24732.34
  expected_time <- 3876.0
  expected_calories <- 924
  expected_max_altitude <- 78.5999984741211
  expected_total_ascent <- 452.5999946594238
  expected_total_descent <- 414.9999942779541
  expected_avg_speed <- 22.97121362229102
  expected_max_speed <- 52.5
  expected_max_hr <- 182
  expected_avg_hr <- 167.4795994065282

  expect_type(result, "list")
  expect_true("summary" %in% names(result))
  expect_true("raw_data" %in% names(result))

  expect_equal(result$summary$total_distance_meters, expected_distance, tolerance = 0.01)
  expect_equal(result$summary$total_time_seconds, expected_time, tolerance = 0.01)
  expect_equal(result$summary$total_calories, expected_calories, tolerance = 0.01)
  expect_equal(result$summary$max_altitude, expected_max_altitude, tolerance = 0.01)
  expect_equal(result$summary$total_ascent, expected_total_ascent, tolerance = 0.01)
  expect_equal(result$summary$total_descent, expected_total_descent, tolerance = 0.01)
  expect_equal(result$summary$average_speed_kmh, expected_avg_speed, tolerance = 0.01)
  expect_equal(result$summary$max_speed_kmh, expected_max_speed, tolerance = 0.01)

  expect_true(is.na(result$summary$max_watts))
  expect_true(is.na(result$summary$max_cadence))
  expect_true(is.na(result$summary$average_cadence))

  expect_equal(result$summary$max_hr, expected_max_hr, tolerance = 0.01)
  expect_equal(result$summary$average_hr, expected_avg_hr, tolerance = 0.01)

  expect_s3_class(result$raw_data, "data.frame")
  expect_true(nrow(result$raw_data) > 0)
})
