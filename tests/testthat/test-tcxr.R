library(tcxr)
library(testthat)

# TCX test files
# TODO: ADD REFERENCE
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
  expect_type(result$max_watts, "double")
  expect_type(result$average_watts, "double")
})


test_that("TCXRead returns expected values for a known TCX test file", {
  result <- TCXRead(sample_tcx_file)
  
  expected_distance <- 134500.8
  expected_time <- 15496
  expected_calories <- 3245
  expected_max_altitude <- 666.2
  expected_max_watts <- 587
  expected_avg_watts <- 227.2201
  
  expect_equal(result$total_distance_meters, expected_distance, tolerance = 0.01)
  expect_equal(result$total_time_seconds, expected_time, tolerance = 0.01)
  expect_equal(result$total_calories, expected_calories, tolerance = 0.01)
  expect_equal(result$max_altitude, expected_max_altitude, tolerance = 0.01)
  expect_equal(result$max_watts, expected_max_watts, tolerance = 0.01)
  expect_equal(result$average_watts, expected_avg_watts, tolerance = 0.01)
})

