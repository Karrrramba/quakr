library(testthat)
library(ggplot2)
library(grid)

test_that("GeomTimelineLabel creates expected grobs with default parameters", {

  p <- ggplot(data.frame(x = as.Date('2000-01-01') + 0:10, label = letters[1:11])) +
    geom_timeline(aes(x = x, xmin = x - 1, xmax = x + 1)) +
    geom_timeline_label(aes(x = x, label = label))

  ggplot_built <- ggplot_build(p)
  panel_params <- ggplot_built$layout$panel_params[[1]]

  data <- data.frame(
    x = as.Date('2000-01-01') + 0:10,
    label = letters[1:11],
    linecolour = "black",
    textcolour = "black",
    alpha = 1
  )

  coord <- ggplot2::coord_cartesian()
  coords <- coord$transform(data, panel_params)

  grobs <- GeomTimelineLabel$draw_group(data, panel_params, coord)

  expect_s3_class(grobs, "gList")
  expect_true(inherits(grobs[[1]], "grob"))
  expect_true(inherits(grobs[[2]], "text"))
})


test_that("GeomTimelineLabel applies default aesthetics", {

  default_aes <- default_aes <- GeomTimelineLabel$default_aes

  expect_equal(default_aes$linecolour, "black")
  expect_equal(default_aes$textcolour, "black")
  expect_equal(default_aes$alpha, 1)
})


test_that("GeomTimelineLabel handles n_max parameter", {
  data <- data.frame(
    x = as.Date('2000-01-01') + 0:10,
    label = letters[1:11],
    limit_var = 1:11,
    size = 3
  )

  p <- ggplot(data) +
    geom_timeline_label(aes(x = x, label = label, limit_var = limit_var), n_max = 5)

  ggplot_built <- ggplot_build(p)
  panel_params <- ggplot_built$layout$panel_params[[1]]

  grobs <- GeomTimelineLabel$draw_group(data, panel_params, ggplot2::CoordCartesian(), n_max = 5)

  text_grob <- grobs[[2]]
  expect_true(length(text_grob$label) <= 5)
})


test_that("GeomTimelineLabel handles label_dodge parameter", {
  data <- data.frame(
    x = as.Date('2000-01-01') + 0:10,
    label = letters[1:11]
  )

  p <- ggplot(data) +
    geom_timeline_label(aes(x = x, label = label), label_dodge = TRUE)

  ggplot_built <- ggplot_build(p)
  panel_params <- ggplot_built$layout$panel_params[[1]]

  grobs <- GeomTimelineLabel$draw_group(data, panel_params, ggplot2::CoordCartesian(), label_dodge = TRUE)

  expect_s3_class(grobs, "gList")
  expect_true(inherits(grobs[[1]], "grobTree"))
  expect_true(inherits(grobs[[2]], "text"))

  expect_true(all(grobs[[2]]$y[c(TRUE, FALSE)] > grobs[[2]]$y[c(FALSE, TRUE)]))
})
