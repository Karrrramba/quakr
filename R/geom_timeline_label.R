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
                          n_max = NA,
                          max_var = NULL,
                          label_dodge = FALSE,
                          check_overlap = FALSE,
                          size.unit = "mm",
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
                    size.unit = size.unit,
                    n_max = n_max,
                    max_var = max_var,
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
                        required_aes = "label",
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
                          print(head(max_var))

                          if (!is.null(nmax) && is.null(max_var)) {
                            cli::cli_abort(c(
                              "Both {.arg n_max} and {.arg max_var} must be specified",
                              "i" = "It looks like you want to use {.arg n_max} but
                                have not mapped any data to {.arg max_var}."
                            )
                            )
                          }

                          if (!is.null(max_var)) {
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
                              x = grid::unit(coords$x[i], "npc"),
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

resolve_text_unit <- function(unit) {
  unit <- rlang::arg_match0(unit, c("mm", "pt", "cm", "in", "pc"))
  switch(
    unit,
    "mm" = .pt,
    "cm" = .pt * 10,
    "in" = 72.27,
    "pc" = 12,
    1
  )
}

mexico %>%
 eq_clean_data() %>%
 filter(lubridate::year(DATE) >= 1990) %>%
 ggplot() +
 geom_timeline(aes(x = DATE,
                   xmin = min(DATE),
                   xmax = max(DATE)
                   )
 ) +
  geom_timeline_label(aes(label = MAG, max_var = COUNTRY), n_max = 3)



