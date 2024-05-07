GeomTimeline <- ggplot2::ggproto("GeomTimeline", Geom,
                           required_aes = c("x", "xmin", "xmax"),
                           default_aes = aes(line_color = "grey",
                                             linewidth =  0.5,
                                             point_color = "lightblue",
                                             alpha = 1),

                           draw_key = draw_key_point,

                           draw_group = function(data, panel_params, coord) {

                             first_row <- data[1, ]

                             if (first_row$group >= 1) {
                               data$y <- data$group
                             } else {
                               data$y <- 0.5
                             }

                             coords <- coord$transform(data, panel_params)
                             print(head(coords))
                             # print(head(data))

                             hline_grob <- grid::segmentsGrob(
                               x0 = grid::unit(coords$xmin, "npc"),
                               y0 = grid::unit(coords$y, "npc"),
                               x1 = grid::unit(coords$xmax, "npc"),
                               y1 = grid::unit(coords$y, "npc"),
                               gp = grid::gpar(col = first_row$line_color,
                                               lwd = first_row$linewidth * .pt,
                                               lty = 1)
                             )

                             # draw points
                             points_grob <- grid::pointsGrob(
                               coords$x,
                               coords$y,
                               pch = 19,
                               gp = grid::gpar(col = first_row$point_colour,
                                               alpha = first_row$alpha,
                                               size = 1)
                             )

                             grid::grobTree(hline_grob,points_grob)
                           }
)


geom_timeline <- function(mapping = NULL,
                          data = NULL,
                          stat = "identity",
                          position = "identity",
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE,
                          ...) {
  ggplot2::layer(
    geom = GeomTimeline,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm,
                  point_color = point_color,
                  ...)
  )
}

eq_clean %>%
  filter(COUNTRY == "CHINA" & lubridate::year(DATE) >= 1990) %>%
  ggplot(aes(x = lubridate::year(DATE)))+
  geom_timeline(aes(xmin = min(lubridate::year(DATE)), xmax = max(lubridate::year(DATE)), point_color = "red"))
