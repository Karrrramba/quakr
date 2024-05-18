#' Add labels to geom_timeline
#'
#' This function adds label to the timeline geom. The distance between the timeline
#' and the labels is static when no `size` aesthetic is specified and calculated
#' based on max point size otherwise.
#'
#' @section Aesthetics: `geom_timeline_label()` understands the following
#'   aesthetics (required aesthetics are in bold):
#'   \itemize{\item{**`x`**} \item{**`label`**}
#'   \item{`linewidth`} \item{`linetype`} \item{`linecolour`}}
#'   \item{`alpha`} \item{`colour`} \item{`shape`} \item{`size`}
#'   \item{`stroke`}
#' @param x A date vector specifying x values.
#' @param label A vector specifying the information to be displayed.
#' @param n_max An integer value that limits the number of
#'  labels based on the values mapped to `limit_var`. If not defined all labels
#'  are displayed. Defaults to `NULL`.
#' @param limit_var A variable used in conjunction with `n_max` to limit
#'   the number of displayed labels to the equal `n_max`, based on the highest
#'   values of the `limit_var`. Defaults to `NULL`.
#' @param label_dodge Logical toggles vertically alternating orientation of
#'  labels to reduce overlapping. Defaults to `FALSE`.
#' @inheritParams ggplot2::GeomText
#'
#' @return A layer \code{ggproto} object.
#'
#' @importFrom cli cli_abort
#' @importFrom ggplot2 aes alpha draw_key_text ggplot_build ggproto last_plot layer size_unit
#' @importFrom grid gpar gList linesGrob textGrob unit
#'
#'
#' @examples
#' p <- southamerica %>%
#' filter(country == "CHILE" & year(date) >= 1970) %>%
#'  ggplot(aes(x = date)) +
#'  geom_timeline(aes(
#'   xmin = min(date),
#'   xmax = max(date)
#'   )
#'  )
#'
#' p + geom_timeline_label(aes(label = location))
#'
#' #use `label_dodge` to reduce overlapping
#' p + geom_timeline_label(aes(label = location), label_dodge = TRUE)
#'
#' #limit number of displayed labels based on earthquake magnitude
#' p + geom_timeline_label(aes(label = mag))
#' p + geom_timeline_label(aes(label = mag, limit_var = mag), n_max = 3)
#'
#' @export
geom_timeline_label <- function(mapping = NULL,
                          data = NULL,
                          stat = "identity",
                          position = "identity",
                          na.rm = FALSE,
                          show.legend = NA,
                          n_max = NULL,
                          limit_var = NULL,
                          label_dodge = FALSE,
                          check_overlap = FALSE,
                          ...,
                          inherit.aes = TRUE)
  {
    ggplot2::layer(
      geom = GeomTimelineLabel,
      mapping = mapping,
      data = data,
      stat = stat,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(na.rm = na.rm,
                    check_overlap = check_overlap,
                    n_max = n_max,
                    label_dodge = label_dodge,
                    ...)
    )
}
# ggproto ------------------------------------------------------

#' @format NULL
#' @usage NULL
#' @export
#' @rdname geom_timeline_label
GeomTimelineLabel <- ggplot2::ggproto("GeomTimelineLabel", Geom,
                        required_aes = c("x", "label"),
                        default_aes = ggplot2::aes(
                          limit_var = NULL,
                          linecolour = "black",
                          textcolour = "black",
                          alpha = 1
                        ),

                        draw_key = ggplot2::draw_key_text,

                        draw_group = function(data, panel_params, coord,
                                              rot = 15,
                                              check_overlap = FALSE,
                                              n_max = NULL,
                                              label_dodge = FALSE
                                              ) {

                          first_row <- data[1, ]

                          if ((!is.null(n_max) && is.numeric(n_max)) && !is.null(first_row$limit)) {
                            data <- data[order(data$limit_var, decreasing = TRUE), ][1:as.integer(n_max), ]
                          }

                          size <- ggplot2::ggplot_build(ggplot2::last_plot())$data[[1]]$size
                          y_scaler <- ifelse(max(size) == min(size), 0.15, 0.03 * max(size))

                          if (is.null(first_row$y)) {
                            data$y <- 0.5
                          }

                          data$yend <- data$y + y_scaler

                          if (label_dodge == TRUE) {
                            data$yend[seq(1, nrow(data), 2)] <- data$y[seq(1, nrow(data), 2)] - y_scaler
                          }


                          coords <- coord$transform(data, panel_params)

                          line_grobs <- lapply(seq_len(nrow(coords)), function(i) {
                            grid::linesGrob(
                              x = grid::unit(c(coords$x[i], coords$x[i]), "npc"),
                              y = grid::unit(c(coords$y[i], coords$yend[i]), "npc"),
                              gp = grid::gpar(
                                col = ggplot2::alpha(coords$linecolour[i], coords$alpha[i])
                                )
                            )
                          })

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
                              fontsize = data$size,
                              fontfamily = data$family),
                            check.overlap = check_overlap
                          )

                          grid::gList(lines_tree, text_grob)
                        }
)
