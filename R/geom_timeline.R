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
                          ...
                          ) {

  ggplot2::layer(
    geom = GeomTimeline,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm,
                  ...
    )
  )
}

# ggproto ------------------------------------------------------

#' @rdname geom_timeline
#' @format NULL
#' @usage NULL
#' @export
GeomTimeline <- ggplot2::ggproto("GeomTimeline", Geom,
                                 required_aes = c("x", "xmin", "xmax"),
                                 default_aes = aes(
                                   lty = 1,
                                   lwd = 0.5,
                                   colour = "grey30",
                                   size = 1,
                                   alpha = 0.25
                                 ),

                                 draw_key = draw_key_point,

                                 draw_group = function(data, panel_params, coord, ...) {

                                   first_row <- data[1, ]
                                   print(first_row)

                                   if (first_row$group >= 1) {
                                     data$y <- data$group
                                   } else {
                                     data$y <- 0.5
                                   }

                                   coords <- coord$transform(data, panel_params)
                                   print(coords)

                                   hline_grob <- grid::segmentsGrob(
                                     x0 = grid::unit(coords$xmin, "npc"),
                                     y0 = grid::unit(coords$y, "npc"),
                                     x1 = grid::unit(coords$xmax, "npc"),
                                     y1 = grid::unit(coords$y, "npc"),
                                     gp = grid::gpar(
                                       lwd = first_row$linewidth,
                                       lty = first_row$linetype
                                     )
                                   )

                                   points_grob <- grid::pointsGrob(
                                     coords$x,
                                     coords$y,
                                     pch = 19,
                                     gp = grid::gpar(
                                       col = coords$colour,
                                       alpha = coords$alpha,
                                       pointsize = coords$size
                                     )
                                   )

                                   grid::gList(hline_grob, points_grob)
                                 }
)

str(eq_clean)
eq_clean %>%
  filter(COUNTRY == "CHINA" & lubridate::year(DATE) >= 1990) %>%
  ggplot(aes(x = lubridate::year(DATE))) +
  geom_timeline(aes(xmin = min(lubridate::year(DATE)), xmax = max(lubridate::year(DATE)), size = MAG, colour = TOTAL_DEATHS), lty = "dashed", lwd = 2)

GeomTimeline <- ggplot2::ggproto("GeomTimeline", ggplot2::GeomPoint,
                                 required_aes = c("x", "xmin", "xmax"),
                                 default_aes = ggplot2::aes(
                                   lty = 1,
                                   lwd = 0.5,
                                   colour = "grey30",
                                   size = 1,
                                   alpha = 0.25
                                 ),

                                 draw_key = ggplot2::draw_key_point,

                                 draw_group = function(data, panel_params, coord) {

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
                                       col = first_row$colour,
                                       lwd = first_row$lwd,
                                       lty = first_row$lty
                                     )
                                   )

                                   points_grob <- ggplot2:::ggproto_parent(ggplot2::geom_point)$draw_group(
                                     data, panel_params, coord
                                   )

                                   grid::gList(hline_grob, points_grob)
                                 }
)

