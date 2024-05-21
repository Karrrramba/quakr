#' @importFrom tibble tibble

test_that("eq_location_clean splits country and location", {
  data <- tibble::tibble(location_name = c("USA: California", "JAPAN: Tokyo", NA, "CANADA: Quebec"))
  expected <- tibble::tibble(country = c("USA", "JAPAN", "CANADA"),
                             location = c("California", "Tokyo", "Quebec"))
  result <- eq_location_clean(data)
  expect_equal(result, expected)
})


test_that("eq_location_clean handles quotes and extra spaces", {
  data <- tibble::tibble(location_name = c(' "USA": "New York" ', ' "FRANCE": "Paris" '))
  expected <- tibble::tibble(country = c("USA", "FRANCE"), location = c("New York", "Paris"))
  result <- eq_location_clean(data)
  expect_equal(result, expected)
})


test_that("eq_location_clean handles cases with too many or too few parts", {
  data <- tibble::tibble(location_name = c("GERMANY: Berlin: Mitte", "ITALY: Rome"))
  expected <- tibble::tibble(country = c("GERMANY", "ITALY"), location = c("Berlin: Mitte", "Rome"))
  result <- eq_location_clean(data)
  expect_equal(result, expected)
})


test_that("eq_location_clean handles all lowercase and converts to title case", {
  data <- tibble::tibble(location_name = c("brazil: rio de janeiro", "india: new delhi"))
  expected <- tibble::tibble(country = c("BRAZIL", "INDIA"), location = c("Rio De Janeiro", "New Delhi"))
  result <- eq_location_clean(data)
  expect_equal(result, expected)
})
