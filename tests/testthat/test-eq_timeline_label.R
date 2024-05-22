library(testthat)
library(ggplot2)
library(grid)

test_that("GeomTimelineLabel creates expected grobs with default parameters", {
  library(ggplot2)
  library(grid)

  data <- data.frame(
    x = as.Date('2000-01-01') + 0:10,
    label = letters[1:11],
    linecolour = "black",
    textcolour = "black",
    linealpha = 0.2,
    textalpha = 1
  )

  p <- ggplot(data) +
    geom_timeline(aes(x = x, xmin = x - 1, xmax = x + 1)) +
    geom_timeline_label(aes(x = x, label = label))

  ggplot_built <- ggplot_build(p)
  panel_params <- ggplot_built$layout$panel_params[[1]]
  coord <- ggplot2::coord_cartesian()
  coords <- coord$transform(data, panel_params)
  grobs <- GeomTimelineLabel$draw_group(data, panel_params, coord)

  expect_s3_class(grobs, "gList")
  expect_s3_class(grobs[[1]], "gTree")
  expect_true(inherits(grobs[[1]], "grob"))
  expect_true(inherits(grobs[[2]], "text"))
  expect_s3_class(grobs[[1]]$children[[1]], "lines")
})


test_that("GeomTimelineLabel applies default aesthetics", {

   default_aes <- GeomTimelineLabel$default_aes

  expect_equal(default_aes$linecolour, "black")
  expect_equal(default_aes$textcolour, "black")
  expect_equal(default_aes$linealpha, 0.2)
  expect_equal(default_aes$textalpha, 1)
  expect_equal(default_aes$limit, NULL)
})


test_that("GeomTimelineLabel handles n_max parameter", {
  library(ggplot2)
  library(grid)

  data <- data.frame(
    x = as.Date('2000-01-01') + 0:10,
    label = letters[1:11],
    linecolour = "black",
    textcolour = "black",
    linealpha = 0.2,
    textalpha = 1
  )

  p <- ggplot(data) +
    geom_timeline(aes(x = x, xmin = x - 1, xmax = x + 1)) +
    geom_timeline_label(aes(x = x, label = label), n_max = 5)

  ggplot_built <- ggplot_build(p)
  panel_params <- ggplot_built$layout$panel_params[[1]]
  coord <- ggplot2::coord_cartesian()
  coords <- coord$transform(data, panel_params)
  grobs <- GeomTimelineLabel$draw_group(data, panel_params, coord)

  expect_true(length(grobs[[2]]$children) <= 5)
})


test_that("GeomTimelineLabel throws an error with correct message when n_max is used without limit", {

  sample_data <- data.frame(
    x = 1:5,
    label = letters[1:5],
    y = rep(0.5, 5)
  )

  create_plot <- function(data){
     ggplot(data, aes(x = x, label = label)) +
      geom_timeline(aes(xmin = min(x), xmax = max(x)))+
      geom_timeline_label(n_max = 3)
   }

  expect_snapshot(create_plot(sample_data), error = TRUE)

})


# test_that("GeomTimelineLabel handles label_dodge parameter", {
#   data <- data.frame(
#     x = as.Date('2000-01-01') + 0:10,
#     label = letters[1:11],
#     linecolour = "black",
#     textcolour = "black",
#     linealpha = 1,
#     textalpha = 1
#   )
#
#   p <- ggplot(data) +
#     geom_timeline(aes(x = x, xmin = x - 1, xmax = x + 1)) +
#     geom_timeline_label(aes(x = x, label = label), label_dodge = TRUE)
#
#   ggplot_built <- ggplot_build(p)$data[[2]]
#   panel_params <- ggplot_built$layout$panel_params[[1]]
#
#   coord <- ggplot2::coord_cartesian()
#   coords <- coord$transform(data, panel_params)
#
#   # Ensure 'yend' is added in the data before transformation
#   grobs <- GeomTimelineLabel$draw_group(data, panel_params, coord, label_dodge = TRUE)
#
#   # Check that "yend" is in the transformed data
#   expect_true("yend" %in% names(data))
#
#   # Print transformed coords for debugging
#   print("Coords after transformation:")
#   print(coords)
#
#   # Check that "yend" is in the transformed coords
#   expect_true("yend" %in% names(coords))
#
#   # Extract yend values from the grobs for validation
#   yend_values <- coords$yend
#
#   # Ensure that yend values alternate when label_dodge is TRUE
#   expect_true(any(diff(yend_values) != 0))
# })
