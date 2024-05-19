sample_data <- data.frame(
  location = c("A", "B", NA),
  mag = c(5.5, NA, 6.3),
  total_deaths = c(10, 50, NA)
)

expected_label_string <- c(
  "<b>Location:</b> A <br/><b>Magnitude:</b> 5.5 <br/><b>Total deaths:</b> 10",
  "<b>Location:</b> B <br/><b>Total deaths:</b> 50",
  "<b>Magnitude:</b> 6.3 <br/>"
  )

# Test 1: Function runs without errors
test_that("eq_create_label runs without errors", {
  expect_error(eq_create_label(sample_data), NA)
})

# Test 2: Function returns a character vector
test_that("eq_create_label returns a character vector", {
  labels <- eq_create_label(sample_data)
  expect_type(labels, "character")
})

# Test 3: Function returns correctly formatted labels
test_that("eq_create_label returns correctly formatted labels", {
  labels <- eq_create_label(sample_data)
  expect_equal(labels, expected_label_string)
})

# Test 4: Function handles NA values correctly
test_that("eq_create_label handles NA values correctly", {
  na_data <- data.frame(
    location = NA,
    mag = NA,
    total_deaths = NA
  )
  labels <- eq_create_label(na_data)
  expect_equal(labels, "")
})

# Test 5: Function works with additional rows
test_that("eq_create_label works with additional rows", {
  additional_data <- data.frame(
    location = c("C", NA),
    mag = c(4.7, 3.2),
    total_deaths = c(0, 5)
  )
  additional_expected_output <- c(
    "<b>Location:</b> C <br/><b>Magnitude:</b> 4.7 <br/><b>Total deaths:</b> 0",
    "<b>Magnitude:</b> 3.2 <br/><b>Total deaths:</b> 5"
  )

  labels <- eq_create_label(additional_data)
  expect_equal(labels, additional_expected_output)
})

# Test 6: Function works with empty data
test_that("eq_create_label works with empty data", {
  empty_data <- data.frame(
    location = character(0),
    mag = numeric(0),
    total_deaths = integer(0)
  )
  labels <- eq_create_label(empty_data)
  expect_equal(labels, character(0))
})
