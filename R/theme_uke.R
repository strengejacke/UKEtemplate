#' @export
theme_uke <- function(base_size = 11,
                      base_family = "Source Sans Pro",
                      title_family = "Roboto Slab",
                      plot.title.size = 1.35 * base_size,
                      plot.title.face = "plain",
                      plot.title.space = 1.8 * base_size,
                      plot.title.position = "plot",
                      legend.position = "right",
                      axis.title.space = 1.8 * base_size,
                      legend.title.size = 1.2 * base_size,
                      legend.text.size = 1.1 * base_size,
                      axis.title.size = 1.2 * base_size,
                      axis.title.face = "plain",
                      axis.text.size = 1.1 * base_size,
                      axis.text.angle = NULL,
                      tags.size = 1.35 * base_size,
                      tags.face = "bold",
                      ...) {

  color_bg <- "#EFF2F4"
  color_strip <- color_axisline <- "#7CA5BF"
  color_strip_text <- "#EDEFF2"
  color_gridline <- "#E3E5F2"

  theme_modern(
    base_size = base_size,
    base_family = base_family,
    plot.title.size = plot.title.size,
    plot.title.face = plot.title.face,
    plot.title.space = plot.title.space,
    plot.title.position = plot.title.position,
    legend.position = legend.position,
    axis.title.space = axis.title.space,
    legend.title.size = legend.title.size,
    legend.text.size = legend.text.size,
    axis.title.size = axis.title.size,
    axis.title.face = axis.title.face,
    axis.text.size = axis.text.size,
    axis.text.angle = axis.text.angle,
    tags.size = tags.size,
    tags.face = tags.face
  ) +
    theme(
      plot.title = element_text(family = title_family),
      plot.background = element_rect(fill = color_bg),
      plot.margin = unit(c(1, 1, 1, 1), "cm"),
      panel.background = element_rect(fill = color_bg),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_line(colour = color_gridline),
      panel.grid.major.y = element_line(colour = color_gridline),
      strip.background = element_rect(colour = color_bg,fill = color_strip, linewidth = 0),
      strip.text = element_text(colour = color_strip_text, face = "plain", size = base_size),
      axis.line.y = element_blank(),
      axis.line.x.bottom = element_line(color = color_axisline, linewidth = 0.75),
      axis.ticks.x.bottom = element_line(linewidth = 0.75, colour = color_axisline),
      axis.ticks.length.x.bottom = unit(-0.25, "cm"),
      axis.text.x.bottom = element_text(margin = margin(t = base_size * 1.3, r = 0, b = 0, l = 0)),
      axis.text.y.left = element_text(margin = margin(t = 0, r = base_size * 1.3, b = 0, l = 0)),
      legend.background = element_rect(fill = color_bg),
      ...
    )
}
