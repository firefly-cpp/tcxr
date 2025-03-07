library(tcxr)
library(XML)
library(testthat)

# TCX test files
# Reference: I. Jr. Fister, S. Rauter, D. Fister, I. Fister. A collection of sport activity datasets with an emphasis on powermeter data. Technical report, University of Maribor, 2017.
# More info: https://github.com/firefly-cpp/tcx-test-files

sample_tcx_file <- "test_files/swimming_activity_1.tcx"

test_that("TCXRead returns expected values for a swimming tcx file)", {
  result <- TCXRead(sample_tcx_file)

  expected_distance <- 1330.32
  expected_time <- 1865.0
  expected_calories <- 284
  expected_max_altitude <- 16.399999618530273
  expected_max_watts <- NA
  expected_max_cadence <- NA
  expected_avg_cadence <- NA
  expected_max_hr <- 147
  expected_avg_hr <- 120

  expect_equal(result$total_distance_meters, expected_distance, tolerance = 0.01)
  expect_equal(result$total_time_seconds, expected_time, tolerance = 0.01)
  expect_equal(result$total_calories, expected_calories, tolerance = 0.01)
  expect_equal(result$max_altitude, expected_max_altitude, tolerance = 0.01)
  expect_equal(result$max_watts, expected_max_watts, tolerance = 0.01)
  expect_equal(result$max_cadence, expected_max_cadence, tolerance = 0.01)
  expect_equal(result$average_cadence, expected_avg_cadence, tolerance = 0.01)
  expect_equal(result$max_hr, expected_max_hr, tolerance = 0.01)
  expect_equal(result$average_hr, expected_avg_hr, tolerance = 0.01)
})
