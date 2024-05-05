GeomTimeline <- ggproto("GeomTimeline", Geom,
                           required_aes = c("x", "xmin", "xmax"),
                           default_aes = aes(shape = 19,
                                             colour = "lightblue",
                                             size = 1.5,
                                             alpha = 0.5,
                                             line_color = "grey"),

                           draw_key = draw_key_point,

                           draw_group = function(data, panel_params, coord) {

                             first_row <- data[1, ]

                             if (first_row$group >= 1) {
                               data$y <- data$group
                             } else {
                               data$y <- 0.5
                             }

                             coords <- coord$transform(data, panel_params)
                             # print(head(coords))
                             # print(head(data))

                             hline_grob <- grid::segmentsGrob(
                               x0 = unit(coords$xmin, "npc"),
                               y0 = unit(coords$y, "npc"),
                               x1 = unit(coords$xmax, "npc"),
                               y1 = unit(coords$y, "npc"),
                               gp = grid::gpar(col = coords$line_color)
                             )

                             # draw points
                             points_grob <- grid::pointsGrob(
                               coords$x,
                               coords$y,
                               pch = coords$shape,
                               gp = grid::gpar(col = alpha(coords$colour, alpha = coords$alpha),
                                               size = coords$size)
                             )
                             ggplot2:::ggname("geom_comp",
                                              grobTree(hline_grob,
                                                       points_grob))
                           }
)

geom_timeline <- function(mapping = NULL,
                              data = NULL,
                              stat = "identity",
                              position = "identity",
                              na.rm = FALSE,
                              show.legend = NA,
                              inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomTimeline,
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
