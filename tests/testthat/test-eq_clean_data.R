test_that("eq_clean_data handles missing locations correctly", {
  data <- tibble::tibble(
    location_name = c("USA: California", "JAPAN: Tokyo", NA, "CANADA: Quebec"),
    year = c(1994, 2011, 2000, 1988),
    mo = c(1, 3, 7, 5),
    dy = c(17, 11, 1, 5),
    latitude = c(34.0522, 35.6895, 45.4215, 46.8139),
    longitude = c(-118.2437, 139.6917, -75.6972, -71.2082)
  )

  expected <- tibble::tibble(
    country = c("USA", "JAPAN", "CANADA"),
    location = c("California", "Tokyo", "Quebec"),
    date = lubridate::ymd(c("1994-01-17", "2011-03-11", "1988-05-05")),
    latitude = c(34.0522, 35.6895, 46.8139),
    longitude = c(-118.2437, 139.6917, -71.2082)
  )

  result <- eq_clean_data(data)
  expect_equal(result, expected)
})


test_that("eq_clean_data returns the correct column names", {
  data <- tibble::tibble(
    location_name = c("USA: California", "JAPAN: Tokyo", NA, "CANADA: Quebec"),
    year = c(1994, 2011, 2000, 1988),
    mo = c(1, 3, 7, 5),
    dy = c(17, 11, 1, 5),
    latitude = c(34.0522, 35.6895, 45.4215, 46.8139),
    longitude = c(-118.2437, 139.6917, -75.6972, -71.2082)
  )

  expected_colnames <- c("country", "location", "date", "latitude", "longitude")

  result <- eq_clean_data(data)
  expect_equal(colnames(result), expected_colnames)
})


test_that("date column is of class Date", {
  data <- tibble::tibble(
    location_name = c("USA: California", "JAPAN: Tokyo"),
    year = c(1994, 2011),
    mo = c(1, 3),
    dy = c(17, 11),
    latitude = c(34.0522, 35.6895),
    longitude = c(-118.2437, 139.6917)
  )

  result <- eq_clean_data(data)
  expect_s3_class(result$date, "Date")
})
