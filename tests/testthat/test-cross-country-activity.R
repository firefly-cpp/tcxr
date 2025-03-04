library(tcxr)
library(XML)
library(testthat)

# TCX test files
# Reference: I. Jr. Fister, S. Rauter, D. Fister, I. Fister. A collection of sport activity datasets with an emphasis on powermeter data. Technical report, University of Maribor, 2017.
# More info: https://github.com/firefly-cpp/tcx-test-files

sample_tcx_file <- "test_files/cross-country-skiing_activity_1.tcx"

test_that("TCXRead returns expected values for a cross country skiing file)", {
  result <- TCXRead(sample_tcx_file)

  expected_distance <- 5692.01
  expected_time <- 2401.26
  expected_calories <- 532
  expected_max_altitude <- 2337.6
  expected_total_ascent <- 1118
  expected_total_descent <- 117.1997
  expected_avg_speed <- 22.97121362229102
  expected_max_speed <- 22.1688 # NOTE: compare with https://github.com/alenrajsp/tcxreader
  expected_max_watts <- NA
  expected_max_cadence <- NA
  expected_avg_cadence <- NA
  expected_max_hr <- 172
  expected_avg_hr <- 137.1081 # NOTE: minor difference with tcxreader

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

