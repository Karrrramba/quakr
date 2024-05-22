test_that("geom_timeline can be added to a ggplot object", {
  p <- ggplot2::ggplot(data.frame(x = as.Date('2000-01-01') + 0:10, xmin = as.Date('2000-01-01'), xmax = as.Date('2000-01-11'))) +
    geom_timeline(aes(x = x, xmin = xmin, xmax = xmax))

  expect_s3_class(p, "gg")
})


test_that("geom_timeline recognizes required aesthetics", {
  library(ggplot2)

  p <- ggplot2::ggplot(data.frame(x = as.Date('2000-01-01') + 0:10, xmin = as.Date('2000-01-01'), xmax = as.Date('2000-01-11'))) +
    geom_timeline(aes(x = x, xmin = xmin, xmax = xmax))

  layer_data <- ggplot2::layer_data(p)
  expect_true(all(c("x", "xmin", "xmax") %in% names(layer_data)))
})


test_that("geom_timeline applies default aesthetics", {
  library(ggplot2)

  p <- ggplot2::ggplot(data.frame(x = as.Date('2000-01-01') + 0:10, xmin = as.Date('2000-01-01'), xmax = as.Date('2000-01-11'))) +
    geom_timeline(aes(x = x, xmin = xmin, xmax = xmax))

  layer <- ggplot2::ggplot_build(p)$data[[1]]
  expect_equal(layer$linetype, rep(1, 11))
  expect_equal(layer$linewidth, rep(0.5, 11))
  expect_equal(layer$linecolour, rep("black", 11))
  expect_equal(layer$colour, rep("black", 11))
  expect_equal(layer$size, rep(3, 11))
  expect_equal(layer$alpha, rep(0.25, 11))
  expect_equal(layer$shape, rep(19, 11))
  expect_equal(layer$fill, rep(NA, 11))
})


test_that("geom_timeline draw_group function creates expected grobs", {
  library(ggplot2)

  # Create a dummy plot to extract panel_params
  p <- ggplot2::ggplot(data.frame(x = as.Date('2000-01-01') + 0:10, xmin = as.Date('2000-01-01'), xmax = as.Date('2000-01-11'))) +
    geom_timeline(aes(x = x, xmin = xmin, xmax = xmax))

  ggplot_built <- ggplot2::ggplot_build(p)
  panel_params <- ggplot_built$layout$panel_params[[1]]

  data <- data.frame(
    x = as.Date('2000-01-01') + 0:10,
    xmin = as.Date('2000-01-01'),
    xmax = as.Date('2000-01-11'),
    y = rep(NA, 11),
    linetype = 1,
    linewidth = 0.5,
    linecolour = "black",
    colour = "black",
    size = 1.75,
    alpha = 0.25,
    shape = 19,
    fill = NA
  )

  coord <- ggplot2::coord_cartesian()
  coords <- coord$transform(data, panel_params)

  grobs <- GeomTimeline$draw_group(data, panel_params, coord)

  expect_s3_class(grobs, "gList")
  expect_true(inherits(grobs[[1]], "segments"))
  expect_true(inherits(grobs[[2]], "points"))
})

# test_that("geom_timeline returns line plot with points", {
#   library(ggplot2)
#   library(vdiffr)
#
#   setwd(system.file("extdata", package = "quakr"))
#
#   data <- data.frame(date = as.Date('2000-01-01') + 0:9,
#                     country = rep(LETTERS[1:2], 5))
#
#   p <- ggplot2::ggplot(data.frame(x = as.Date('2000-01-01') + 0:10, xmin = as.Date('2000-01-01'), xmax = as.Date('2000-01-11'))) +
#     geom_timeline(aes(x = x, xmin = xmin, xmax = xmax))
#
#   expect_doppelganger("reference map", ref_plot)
#
#   output_plot <- fars_map_state(state, year)
#   expect_doppelganger("default map", output_plot)
# })
