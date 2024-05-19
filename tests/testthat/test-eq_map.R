library(testthat)
library(leaflet)

# Sample data for testing
sample_data <- data.frame(
  longitude = c(-118.2437, -122.4194, -73.935242),
  latitude = c(34.0522, 37.7749, 40.730610),
  mag = c(4.5, 5.0, 3.2),
  annotation = c("LA", "SF", "NY")
)


test_that("eq_map runs without errors", {
  expect_error(eq_map(sample_data, "annotation"), NA)
})


test_that("eq_map returns a leaflet map object", {
  map <- eq_map(sample_data, "annotation")
  expect_true(inherits(map, "leaflet"))
})


test_that("eq_map uses scaling_factor correctly", {
  map1 <- eq_map(sample_data, "annotation", scaling_factor = 1)
  map2 <- eq_map(sample_data, "annotation", scaling_factor = 2)

  radius1 <- sample_data$mag * (1 * 1000)
  radius2 <- sample_data$mag * (2 * 1000)

  expect_true(all(map1$x$calls[[2]]$args[[3]] == radius1))
  expect_true(all(map2$x$calls[[2]]$args[[3]] == radius2))
})


test_that("eq_map uses annot_col correctly", {
  map <- eq_map(sample_data, "annotation")
  expect_true(all(map$x$calls[[2]]$args[[7]] == sample_data$annotation))
})


test_that("eq_map adds circles with correct properties", {
  map <- eq_map(sample_data, "annotation")

  circle_call <- map$x$calls[[2]]
  expect_equal(circle_call$method, "addCircles")
  expect_equal(circle_call$args[[1]], sample_data$latitude)
  expect_equal(circle_call$args[[2]], sample_data$longitude)
  expect_equal(circle_call$args[[3]], sample_data$mag * 1000)
  expect_equal(circle_call$args[[6]]$fillColor, "darkred")
  expect_equal(circle_call$args[[6]]$fillOpacity, 0.4)
})
