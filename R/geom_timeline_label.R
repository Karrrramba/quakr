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
#'  labels based on the values mapped to `max_var`. If not defined all labels
#'  are displayed. Defaults to `NULL`.
#' @param max_var A numeric vector used in conjunction with `n_max` to reduce
#'  labels to the `n_max` highest values. Defaults to the "mag" column.
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
#' mexico %>%
#'  eq_clean_data() %>%
#'  filter(lubridate::year(date) >= 1990) %>%
#'  ggplot() +
#'  geom_timeline(aes(
#'   x = date,
#'   xmin = min(date),
#'   xmax = max(date)
#'   )
#'  ) +
#'  geom_timeline_label(label = location)
#'
#' # use case with `annotate`
#'
geom_timeline_label <- function(mapping = NULL,
                          data = NULL,
                          stat = "identity",
                          position = "identity",
                          na.rm = FALSE,
                          show.legend = NA,
                          n_max = NULL,
                          max_var = "mag",
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
                    # max_var = max_var,
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
                          linecolour = "grey",
                          textcolour = "black",
                          max_var = NULL,
                        ),

                        draw_key = draw_key_text,

                        draw_group = function(data, panel_params, coord,
                                              check_overlap = FALSE, size.unit = "mm",
                                              n_max = NULL,
                                              label_dodge = FALSE
                                              ) {

                          first_row <- data[1, ]
                          if (!is.numeric(n_max)) {
                            cli::cli_abort("{.arg n_max} must be numeric.")
                          } else {
                          nmax <- as.integer(n_max)
                          }

                          max_var <- first_row$max_var
                          print(head(data))

                          if (!is.null(nmax)) {
                            data <- data[order(data$max_var, decreasing = TRUE), ]
                            data <- data[1:nmax, ]
                          }

                          if (is.null(first_row$y)) {
                            data$y <- 0.5
                            data$yend <- 0.55
                          } else {
                            data$yend <- data$y + 0.1
                          }

                          if (label_dodge == TRUE) {
                            data$yend[seq(1, nrow(data), 2)] <- data$y[seq(1, nrow(data), 2)] - 0.1
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
                          } else {
                            coords$hjust = 0
                          }


                          text_grob <- grid::textGrob(
                            label = coords$label,
                            x = grid::unit(coords$x, "npc"),
                            y = grid::unit(coords$yend, "npc"),
                            rot = 15,
                            hjust = coords$hjust,
                            vjust = 0,
                            gp = grid::gpar(
                              fontsize = data$size,
                              fontfamily = data$family),
                            check_overlap = check_overlap
                          )

                          grid::gList(lines_tree, text_grob)
                        }
)

mexico %>%
 eq_clean_data() %>%
 filter(lubridate::year(date) >= 1990) %>%
 ggplot() +
 geom_timeline(aes(x = date,
                   xmin = min(date),
                   xmax = max(date)
                   )
 ) +
  geom_timeline_label(aes(label = location), n_max = 3)



