library(testthat)

# Sample data for testing
test_data <- data.frame(
  location = c("Location1", "Location2", NA),
  mag = c(5.5, 6.2, 7.0),
  total_deaths = c(10, NA, 50),
  year = c(2001, 2002, 2003)
)

test_that("eq_create_label works with default variables", {
  labels <- eq_create_label(test_data)

  expected_labels <- c(
    "<b>Location:</b> Location1<br/><b>Mag:</b> 5.5<br/><b>Total Deaths:</b> 10",
    "<b>Location:</b> Location2<br/><b>Mag:</b> 6.2",
    "<b>Mag:</b> 7.0<br/><b>Total Deaths:</b> 50"
  )

  expect_equal(labels, expected_labels)
})

test_that("eq_create_label works with custom variables", {
  labels <- eq_create_label(test_data, defaults = FALSE, "location", "year")

  expected_labels <- c(
    "<b>Location:</b> Location1<br/><b>Year:</b> 2001",
    "<b>Location:</b> Location2<br/><b>Year:</b> 2002",
    "<b>Year:</b> 2003"
  )

  expect_equal(labels, expected_labels)
})

test_that("eq_create_label handles missing values correctly", {
  labels <- eq_create_label(test_data)

  expected_labels <- c(
    "<b>Location:</b> Location1<br/><b>Mag:</b> 5.5<br/><b>Total Deaths:</b> 10",
    "<b>Location:</b> Location2<br/><b>Mag:</b> 6.2",
    "<b>Mag:</b> 7.0<br/><b>Total Deaths:</b> 50"
  )

  expect_equal(labels, expected_labels)
})

test_that("eq_create_label works with a mix of default and custom variables", {
  labels <- eq_create_label(test_data, defaults = TRUE, "year")

  expected_labels <- c(
    "<b>Location:</b> Location1<br/><b>Mag:</b> 5.5<br/><b>Total Deaths:</b> 10<br/><b>Year:</b> 2001",
    "<b>Location:</b> Location2<br/><b>Mag:</b> 6.2<br/><b>Year:</b> 2002",
    "<b>Mag:</b> 7.0<br/><b>Total Deaths:</b> 50<br/><b>Year:</b> 2003"
  )

  expect_equal(labels, expected_labels)
})
