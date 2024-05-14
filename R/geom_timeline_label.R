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
#' @param n_max An integer value or "all" (default) that limits the number of
#'  labels based on earthquake magnitude.
#' @param alt_label Logical toggles horizontally alternating orientation of
#'  labels to reduce overlapping. Defaults to `FALSE`.
#' @inheritParams ggplot2::GeomLabel
#'
#' @return A layer \code{ggproto} object.
#'
#' @importFrom cli cli_abort
#' @importFrom ggplot2 aes alpha ggproto layer
#' @importFrom grid gpar gList linesGrob textGrob unit
#'
#' @export
#'
#' @examples
#' data(mexico)
#' mexico %>%
#'  eq_clean_data() %>%
#'  filter(lubridate::year(DATE) >= 1990) %>%
#'  ggplot() +
#'  geom_timeline(aes(
#'   x = DATE,
#'   xmin = min(DATE),
#'   xmax = max(DATE)
#'   )
#'  ) +
#'  geom_timeline_label(label = LOCATION)
#'
#' # use case with `annotate`
#'
geom_timeline_label <- function(mapping = NULL,
                          data = NULL,
                          stat = "identity",
                          position = "identity",
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE,
                          n_max = "all",
                          filter = NULL,
                          filter_desc = TRUE,
                          alt_labels = FALSE,
                          check_overlap = FALSE,
                          ...)
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
                          n_max = "all",
                          filter = NULL,
                          filter_desc = TRUE,
                          linecolour = "grey",
                          textcolour = "black",
                          check_overlap = TRUE,
                          alt_labels = FALSE,
                        ),

                        draw_key = draw_key_text,

                        draw_group = function(data, panel_params, coord) {


                          first_row <- data[1, ]
                          nmax <- first_row[ , "n_max"]

                          if (is.numeric(nmax) && is.null(first_row$filter)) {
                            cli::cli_abort(c(
                              "Both {.arg n_max} and {.arg filter} must be specified",
                              "i" = "It looks like you have not mapped any data to {.arg filter}"
                            ))
                          }

                          if (is.numeric(nmax)) {
                            data$n_max <- as.integer(data$n_max)
                            if (first_row$filter_desc == TRUE) {
                              data <- data[order(data$filter, decreasing = TRUE), ]
                            } else {
                              data <- data[order(data$filter), ]
                            }
                            data <- data[1:nmax, ]
                          } else if(nmax != "all") {
                            cli::cli_abort("{.arg n_max} must be numeric or 'all'")
                          }

                          if (is.null(first_row$y)) {
                            data$y <- 0.5
                            data$yend <- 0.55
                          } else {
                            data$yend <-  data$y + 0.1
                          }

                          if (first_row$alt_labels == TRUE) {
                            data$yend[seq(1, nrow(data), 2)] <- data$y[seq(1, nrow(data), 2)] - 0.1
                          }

                          coords <- coord$transform(data, panel_params)

                          line_grobs <- lapply(seq_len(nrow(coords)), function(i) {
                            grid::linesGrob(
                              x = grid::unit(coords$x[i], "npc"),
                              y = grid::unit(c(coords$y[i], coords$yend[i]), "npc"),
                              gp = grid::gpar(
                                col = ggplot2::alpha(coords$linecolour[i], coords$alpha[i])
                                )
                            )
                          })

                          lines_tree <- do.call("grobTree", line_grobs)

                          if (first_row$alt_labels == TRUE) {
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
                            gp = grid::gpar(size = 1),
                            check.overlap = first_row$check_overlap
                          )

                          grid::gList(lines_tree, text_grob)
                        }
)


eq_clean %>%
  filter(COUNTRY == "CHINA" & lubridate::year(DATE) >= 1990) %>%
  ggplot(aes(x = lubridate::year(DATE)))+
  geom_timeline(aes(xmin = min(lubridate::year(DATE)), xmax = max(lubridate::year(Date)))) +
  geom_timeline_label(aes(label = LOCATION, n_max = 5, filter = MAG, filter_desc = FALSE))


eq_clean %>%
  filter(COUNTRY %in%  c("MEXICO", "JAPAN", "CHINA") & lubridate::year(DATE) >= 1990) %>%
  ggplot(aes(x = lubridate::year(DATE), y = Country)) +
  geom_timeline(aes(xmin = min(lubridate::year(DATE)), xmax = max(lubridate::year(DATE)), size = DEATHS)) +
  geom_timeline_label(aes(label = LOCATION, n_max = "all", alt_labels = TRUE, check_overlap = TRUE))

