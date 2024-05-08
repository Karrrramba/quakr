#' Timeline
#'
#'This geom plot a vertical timeline with overlaid
#'
#' @section Aestethics:
#' This geom supports the same aesthetics as `geom_point` plus
#' the following aesthetics for the horizontal line:
#' `line_color`
#' `linewidth`
#' `linetype`
#'
#' @inheritParams ggplot2::geom_point
#' @param name description
#' @export
#' @param x description
#' @examples
#' # example code
#'

geom_timeline <- function(mapping = NULL,
                          data = NULL,
                          stat = "identity",
                          position = "identity",
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE,
                          ...,
                          line.params = list(),
                          point.params = list()) {

  line_params <- modifyList(list(lwd = 0.5, color = "grey", lty = 1), line.params)
  point_params <- modifyList(list(size = 1.5, color = "lightblue", alpha = 0.3), point.params)

  ggplot2::layer(
    geom = GeomTimeline,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm,
                  ...,
                  line.params = line_params,
                  point.params = point_params)
  )
}

#' @rdname geom_timeline
#' @format NULL
#' @usage NULL
#' @export
GeomTimeline <- ggplot2::ggproto("GeomTimeline", Geom,
                                 required_aes = c("x", "xmin", "xmax"),
                                 default_aes = aes(
                                   line.params = list(color = "grey", lwd = 0.5, lty = 1),
                                   point.params = list(color = "lightblue", alpha = 0.3)
                                 ),

                                 draw_key = draw_key_point,

                                 draw_group = function(data, panel_params, coord, line.params, point.params) {

                                   print(line.params)

                                   if (!is.null(line.params)) {
                                     panel_params <- modifyList(panel_params, line.params)
                                   }

                                   if (!is.null(point.params)) {
                                     panel_params <- modifyList(panel_params, point.params)
                                   }

                                   first_row <- data[1, ]

                                   if (first_row$group >= 1) {
                                     data$y <- data$group
                                   } else {
                                     data$y <- 0.5
                                   }

                                   coords <- coord$transform(data, panel_params)

                                   hline_grob <- grid::segmentsGrob(
                                     x0 = grid::unit(coords$xmin, "npc"),
                                     y0 = grid::unit(coords$y, "npc"),
                                     x1 = grid::unit(coords$xmax, "npc"),
                                     y1 = grid::unit(coords$y, "npc"),
                                     gp = grid::gpar(
                                       col = panel_params$line.params$color,
                                       lwd = panel_params$line.params$lwd * .pt,
                                       lty = panel_params$line.params$lty
                                     )
                                   )

                                   points_grob <- grid::pointsGrob(
                                     coords$x,
                                     coords$y,
                                     pch = 19,
                                     gp = grid::gpar(
                                       col = panel_params$point.params$color,
                                       alpha = panel_params$point.params$alpha,
                                       size = panel_params$point.params$size
                                     )
                                   )

                                   grid::gList(hline_grob, points_grob)
                                 }
)





eq_clean %>%
  filter(COUNTRY == "CHINA" & lubridate::year(DATE) >= 1990) %>%
  ggplot(aes(x = lubridate::year(DATE))) +
  geom_timeline(aes(xmin = min(lubridate::year(DATE)), xmax = max(lubridate::year(DATE))))
