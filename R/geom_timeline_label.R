#' Add timeline labels
#'
#' This function adds label to the timeline geom.
#'
#' @section Aesthetics: `geom_pointpath()` understands the following
#'   aesthetics (required aesthetics are in bold):
#'   \itemize{\item{**`x`**} \item{**`label`**}
#'   \item{`linewidth`} \item{`linetype`} \item{`linecolour`}}
#'   \item{`alpha`} \item{`colour`} \item{`shape`} \item{`size`}
#'   \item{`stroke`}
#' @param x A date vector specifying x values.
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
#' @importFrom ggplot2 aes alpha ggproto layer size_unit
#' @importFrom grid gpar gList linesGrob textGrob unit
#'
#' @export
#'
#' @examples
#' data(mexico)
#' p <- mexico %>%
#'  eq_clean_data() %>%
#'  filter(lubridate::year(date) >= 1990) %>%
#'  ggplot(aes(x = date)) +
#'  geom_timeline(aes(
#'   xmin = min(date),
#'   xmax = max(date)
#'   )
#'  )
#' p + geom_timeline_label(label = location)
#'
#' #limit number of displayed labels
#' p + geom_timeline_label(aes(label = location, limit_var = mag), n_max = 3)
#'
#' # use case with `annotate`
#' p + annotate(
#'   "timeline_label",
#'
#'   )
#'
#'
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
                          linecolour = "grey",
                          textcolour = "black",
                          alpha = 1
                        ),

                        draw_key = draw_key_text,

                        draw_group = function(data, panel_params, coord,
                                              rot = 15,
                                              check_overlap = FALSE,
                                              n_max = NULL,
                                              label_dodge = FALSE
                                              ) {

                          first_row <- data[1, ]
                          limit <- first_row$limit_var

                          if ((!is.null(n_max) && is.numeric(n_max)) && !is.null(limit)) {
                            data <- data[order(data$limit_var, decreasing = TRUE), ][1:as.integer(n_max), ]
                            # data <- data[1:as.integer(n_max), ]
                          }


                          previous_data <- ggplot2::ggplot_build(ggplot2::last_plot())$data[[1]]
                          size <- previous_data$size

                          y_scaler <- ifelse(max(size) == min(size), 0.15, 0.03 * max(size))
                          print(y_scaler)

                          if (is.null(first_row$y)) {
                            data$y <- 0.5
                          }

                          data$yend <- data$y + y_scaler

                          if (label_dodge == TRUE) {
                            data$yend[seq(1, nrow(data), 2)] <- data$y[seq(1, nrow(data), 2)] - y_scaler
                          }


                          coords <- coord$transform(data, panel_params)
                          print(head(coords))

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

southamerica %>%
  eq_clean_data() %>%
  filter(lubridate::year(date) >= 2005 & country %in% c("ARGENTINA", "PERU", "CHILE")) %>%
  ggplot(aes(x = date, y = country)) +
  geom_timeline(aes(
    xmin = min(date),
    xmax = max(date)

  ), size = 3
  ) +
  geom_timeline_label(aes(label = location), n_max = 3, check_overlap = TRUE, label_dodge = TRUE, rot = 15)

