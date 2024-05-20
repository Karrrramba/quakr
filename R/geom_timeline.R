#' Plots a timeline of earthquake events
#'
#' Creates a timeline geom for ggplot2, which can be used to visualize
#' events or intervals over a continuous time axis.
#'
#' @section Aesthetics: `geom_timeline()` understands the following
#'   aesthetics (required aesthetics are in bold):
#'   \itemize{\item{**`x`**} - A date vector specifying the occurrences of earthquakes.
#'   \item{**`xmin`**} - A date value specifying the left boundary of the timeline.
#'   \item{**`xmax`**} - A date value specifying the left boundary of the timeline.
#'   \item{`y`} - A factor specifying any stratification for multiple timelines.
#'     By default one timeline is plotted in the middle of the viewport.
#'   \item{`linewidth`}
#'   \item{`linetype`}
#'   \item{`linecolour`}
#'   \item{`alpha`}
#'   \item{`colour`}
#'   \item{`shape`}
#'   \item{`size`}
#'   \item{`stroke`}
#'   }
#'
#' @inheritParams ggplot2::GeomPoint
#' @inheritParams ggplot2::GeomLine
#' @inheritParams ggplot2::layer
#'
#' @returns A layer \code{ggproto} object.
#'
#' @importFrom ggplot2 aes alpha draw_key_point fill_alpha Geom ggproto layer
#' @importFrom grid gpar gList pointsGrob segmentsGrob unit
#'
#' @examples
#' #single timeline without `y` aesthetic
#' p <- southamerica %>%
#'  filter(country == "CHILE" & year(date) >= 1970) %>%
#'  ggplot(aes(x = date)))
#'
#' p + geom_timeline(aes(xmin = min(date), xmax = max(date))
#'
#' #multiple timelines
#' s <- southamerica %>%
#'  filter(lubridate::year(date) >= 1990) %>%
#'  ggplot(aes(x = date, y = country))
#'
#' s + geom_timeline(aes(xmin = min(date), xmax = max(date))
#'
#' @export

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
GeomTimeline <- ggplot2::ggproto("GeomTimeline", ggplot2::Geom,
                                 required_aes = c("x", "xmin", "xmax"),
                                 default_aes = ggplot2::aes(
                                   y = NA,
                                   linetype = 1,
                                   linewidth = 0.5,
                                   linecolour = "black",
                                   colour = "black",
                                   size = 1.75,
                                   alpha = 0.25,
                                   shape = 19,
                                   fill = NA
                                 ),

                                 non_missing_aes = c("size", "shape", "colour",
                                                     "linetype", "linewidth",
                                                     "linecolour"),

                                 draw_key = ggplot2::draw_key_point,

                                 draw_group = function(data, panel_params, coord, ...) {

                                   first_row <- data[1, ]

                                   coords <- coord$transform(data, panel_params)

                                   if(is.na(first_row$y)) {
                                     coords$y = 0.5
                                   }

                                   line_grob <- grid::segmentsGrob(
                                     x0 = grid::unit(coords$xmin, "npc"),
                                     y0 = grid::unit(coords$y, "npc"),
                                     x1 = grid::unit(coords$xmax, "npc"),
                                     y1 = grid::unit(coords$y, "npc"),
                                     gp = grid::gpar(
                                       lwd = first_row$linewidth,
                                       lty = first_row$linetype,
                                       col = first_row$linecolour
                                     )
                                   )

                                   points_grob <- grid::pointsGrob(
                                     coords$x,
                                     coords$y,
                                     pch = coords$shape,
                                     size = grid::unit(coords$size, "char"),
                                     gp = grid::gpar(
                                       col = ggplot2::alpha(coords$colour, coords$alpha),
                                       fill = ggplot2::fill_alpha(coords$fill, coords$alpha)
                                     )
                                   )

                                   grid::gList(line_grob, points_grob)
                                 }
)
