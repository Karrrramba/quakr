#' Timeline
#'
#'This geom plot a vertical timeline with overlaid
#'
#' @section Aestethics:
#' This geom supports the same aesthetics as `geom_point` plus
#' the following line aesthetics:
#' `linewidth`
#' `linetype`
#' `linecolour`
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
                                   linecolour = "black",
                                   colour = "black",
                                   size = 1,
                                   alpha = 0.25,
                                   shape = 19,
                                   fill = NA
                                 ),

                                 draw_key = draw_key_point,

                                 draw_group = function(data, panel_params, coord, ...) {

                                   first_row <- data[1, ]
                                   print(str(first_row))

                                   if (first_row$group >= 1) {
                                     data$y <- data$group
                                   } else {
                                     data$y <- 0.5
                                   }

                                   coords <- coord$transform(data, panel_params)
                                   print(head(coords))

                                   hline_grob <- grid::segmentsGrob(
                                     x0 = grid::unit(coords$xmin, "npc"),
                                     y0 = grid::unit(coords$y, "npc"),
                                     x1 = grid::unit(coords$xmax, "npc"),
                                     y1 = grid::unit(coords$y, "npc"),
                                     gp = grid::gpar(
                                       lwd = first_row$linewidth,
                                       lty = first_row$linetype,
                                       col = first_row$line_colour
                                     )
                                   )

                                   points_grob <- grid::pointsGrob(
                                     coords$x,
                                     coords$y,
                                     pch = coords$shape,
                                     size = unit(coords$size, "char"),
                                     gp = grid::gpar(
                                       col = ggplot2::alpha(coords$colour, coords$alpha),
                                       fill = ggplot2::fill_alpha(coords$fill, coords$alpha)
                                     )
                                   )

                                   grid::gList(hline_grob, points_grob)
                                 }
)


eq_clean %>%
  filter(COUNTRY == "CHINA" & lubridate::year(DATE) >= 1990 & !is.na(DATE)) %>%
  ggplot(aes(x = DATE)) +
  geom_timeline(aes(xmin = min(DATE), xmax = max(DATE), size = MAG, colour = TOTAL_DEATHS), lty = "dashed", lwd = 2)
