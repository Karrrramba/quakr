library(tidyverse)
library(grid)

GeomTimelineLabel <- ggproto("GeomTimelineLabel", Geom,
                        required_aes = c("x", "label", "mag"),
                        default_aes = aes(n_max = 3,
                                          line_color = "grey",
                                          text_color = "black",
                                          check_overlap = TRUE,
                                          alt_labels = FALSE
                                          ),

                        draw_key = draw_key_text,

                        draw_group = function(data, panel_params, coord) {

                          first_row <- data[1, ]
                          nmax <- first_row[ , "n_max"]

                          if (is.null(first_row$y)) {
                            data$y <- 0.5
                            data$yend <- 0.55
                          } else {
                            data$yend <-  data$y + 0.1
                          }

                          if (first_row$alt_labels == TRUE) {
                            data$yend[seq(1, nrow(data), 2)] <-  data$y[seq(1, nrow(data), 2)] - 0.1
                          }

                          if (is.numeric(nmax)) {
                            data$n_max <- as.integer(data$n_max)
                            data <- data[order(data$mag, decreasing = TRUE), ]
                            data <- data[1:nmax, ]
                          } else if(nmax != "all") {
                            cli::cli_abort("n_max must be numeric or 'all'")
                          }

                          coords <- coord$transform(data, panel_params)

                          # draw line
                          line_grobs <- lapply(seq_len(nrow(coords)), function(i) {
                            grid::linesGrob(x = unit(coords$x[i], "npc"),
                                      y = unit(c(coords$y[i], coords$yend[i]), "npc"),
                                      gp = grid::gpar(col = coords$line_colour[i])
                            )
                          })

                          lines_tree <- do.call("grobTree", line_grobs)

                          # add label text
                          if (first_row$alt_labels == TRUE) {
                            coords$hjust <- ifelse(coords$yend > coords$y, 0, 1)
                          } else {
                            coords$hjust = 0
                          }

                          text_grob <- grid::textGrob(
                            label = coords$label,
                            x = unit(coords$x, "npc"),
                            y = unit(coords$yend, "npc"),
                            rot = 15,
                            hjust = coords$hjust,
                            vjust = 0,
                            check.overlap = TRUE,
                            gp = grid::gpar(size = 1)
                          )

                          grobTree(lines_tree, text_grob)
                        }
)

geom_timeline_label <- function(mapping = NULL,
                          data = NULL,
                          stat = "identity",
                          position = "identity",
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE,
                          ...) {
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

eq_clean %>%
  filter(Country == "CHINA" & lubridate::year(Date) >= 1990) %>%
  ggplot(aes(x = lubridate::year(Date), color = Mag))+
  geom_timeline(aes(xmin = min(lubridate::year(Date)), xmax = max(lubridate::year(Date)), size = Deaths)) +
  geom_timeline_label(aes(label = Location, mag = Mag, n_max = "all"))


eq_clean %>%
  filter(Country %in%  c("MEXICO", "JAPAN", "CHINA") & lubridate::year(Date) >= 1990) %>%
  ggplot(aes(x = lubridate::year(Date), y = Country)) +
  geom_timeline(aes(xmin = min(lubridate::year(Date)), xmax = max(lubridate::year(Date)))) +
  geom_timeline_label(aes(label = Location, mag = Mag, n_max = "all", alt_labels = T))

