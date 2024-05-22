#' Add labels to geom_timeline
#'
#' This function adds label to the timeline geom. The distance between the timeline
#' and the labels is static when no `size` aesthetic is specified and calculated
#' based on max point size otherwise.
#'
#' @section Aesthetics:
#'   `geom_timeline_label()` understands the following
#'   aesthetics (required aesthetics are in bold):
#'
#'   \itemize{\item{**`x`**} - A date vector specifying x values.
#'   \item{**`label`**} - A vector specifying the information to be displayed.
#'   \item{`limit`} - A variable used in conjunction with `n_max` to limit
#'   the number of displayed labels to the equal `n_max`, based on the highest
#'   values of the `limit_var`. Defaults to `NULL`.
#'   \item{`linewidth`}
#'   \item{`linetype`}
#'   \item{`linecolour`}
#'   \item{`alpha`}
#'   \item{`colour`}
#'   \item{`shape`}
#'   \item{`fontsize`}
#'   \item{`stroke`}
#'   }
#'
#' @param n_max An integer value that limits the number of
#'  labels based on the values mapped to `limit`. If not defined all labels
#'  are displayed. Defaults to `NULL`.
#' @param label_dodge Logical toggles vertically alternating orientation of
#'  labels to reduce overlapping. Defaults to `FALSE`.
#' @param check_overlap If `TRUE`, text that overlaps previous text in the
#'  same layer will not be plotted. `check_overlap` happens at draw time and in
#'  the order of the data. Therefore data should be arranged by the label
#'  column before calling `geom_timeline_label()`. Defaults to `FALSE`.
#'
#' @inheritParams ggplot2::GeomText
#' @inheritParams ggplot2::layer
#' @inheritDotParams ggplot2::layer
#'
#' @return A layer \code{ggproto} object.
#'
#' @importFrom cli cli_abort
#' @importFrom dplyr filter
#' @importFrom ggplot2 aes alpha draw_key_text Geom ggplot ggplot_build ggproto last_plot labs layer theme theme_minimal unit
#' @importFrom grid gpar gList grobTree linesGrob textGrob unit
#' @importFrom lubridate year
#'
#' @examples
#' library(ggplot2)
#' library(dplyr)
#'
#' p <- southamerica %>%
#'  filter(country == "CHILE" & lubridate::year(date) >= 1970) %>%
#'  ggplot(aes(x = date)) +
#'  geom_timeline(aes(xmin = min(date),xmax = max(date)))
#'
#' p + geom_timeline_label(aes(label = location))
#'
#' #use `label_dodge` to reduce overlapping
#' p + geom_timeline_label(aes(label = location), label_dodge = TRUE)
#'
#' #limit number of displayed labels based on earthquake magnitude
#' p + geom_timeline_label(aes(label = mag))
#' #p + geom_timeline_label(aes(label = mag, limit = mag), n_max = 3)
#'
#' @export
geom_timeline_label <- function(
    mapping       = NULL,
    data          = NULL,
    stat          = "identity",
    position      = "identity",
    show.legend   = NA,
    n_max         = NULL,
    label_dodge   = FALSE,
    check_overlap = FALSE,
    ...,
    inherit.aes   = TRUE)
  {
    ggplot2::layer(
      geom        = GeomTimelineLabel,
      mapping     = mapping,
      data        = data,
      stat        = stat,
      position    = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      param       = list(
        check_overlap = check_overlap,
        n_max         = n_max,
        label_dodge   = label_dodge,
        ...)
    )
}
# ggproto ------------------------------------------------------

#' @format NULL
#' @usage NULL
#' @export
#' @rdname geom_timeline_label
GeomTimelineLabel <- ggplot2::ggproto(
  "GeomTimelineLabel",
  ggplot2::Geom,
  required_aes = c("x", "label"),
  default_aes = ggplot2::aes(
    limit = NULL,
    linecolour = "lightgrey",
    linealpha = 0.2,
    fontsize = 3,
    textcolour = "black",
    textalpha = 1
  ),

  draw_key = ggplot2::draw_key_text,

  draw_group = function(data,
                        panel_params,
                        coord,
                        rot = 15,
                        check_overlap = FALSE,
                        n_max = NULL,
                        label_dodge = FALSE
  ) {

    first_row <- data[1, ]

    # adjust line length based on the point size

    point_size <- ggplot2::ggplot_build(ggplot2::last_plot())$data[[1]]$size
    y_scaler <- ifelse(max(point_size) == min(point_size), 0.15, 0.04 * max(point_size))

    if (is.null(first_row$y)) {
      data$y <- 0.5
    }

    data$yend <- data$y + y_scaler

    if(!is.null(n_max) && is.numeric(n_max) && is.null(first_row$limit)) {
      cli::cli_abort("{.arg n_max} must be used in conjunction with {.arg limit}.")
    }

    if ((!is.null(n_max) && is.numeric(n_max)) && !is.null(first_row$limit)) {
      data <- data[order(data$limit, decreasing = TRUE), ][1:as.integer(n_max), ]
    } else


    if (label_dodge == TRUE) {
      data$yend[seq(1, nrow(data), 2)] <- data$y[seq(1, nrow(data), 2)] - y_scaler
    }

    coords <- coord$transform(data, panel_params)

    line_grobs <- lapply(seq_len(nrow(coords)), function(i) {
      grid::linesGrob(
         x = grid::unit(c(coords$x[i], coords$x[i]), "npc"),
         y = grid::unit(c(coords$y[i], coords$yend[i]), "npc"),
         gp = grid::gpar(
           col = ggplot2::alpha(coords$linecolour[i], coords$linealpha[i])
         )
      )
     }
    )

    lines_tree <- do.call("grobTree", line_grobs)

    if (label_dodge == TRUE) {
      coords$hjust <- ifelse(coords$yend > coords$y, 0, 1)
      coords$vjust <- ifelse(coords$yend > coords$y, 0, 1)
    } else {
      coords$hjust = 0
      coords$vjust = 0
    }

    text_grob <- grid::textGrob(
      label = coords$label,
      x = grid::unit(coords$x, "npc"),
      y = grid::unit(coords$yend, "npc"),
      rot = rot,
      hjust = coords$hjust,
      vjust = coords$vjust,
      gp = grid::gpar(
        col = ggplot2::alpha(coords$textcolour, coords$textalpha),
        fontsize = data$fontsize,
        fontfamily = data$family
      ),
      check.overlap = check_overlap
     )

     grid::gList(lines_tree, text_grob)
   }
)

