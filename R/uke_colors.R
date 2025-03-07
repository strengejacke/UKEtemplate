#' @export
scale_color_uke <- function(palette = NULL,
                            discrete = TRUE,
                            reverse = FALSE,
                            aesthetics = "color",
                            ...) {
  if (is.null(palette)) {
    palette <- ifelse(discrete, "uke", "gradient")
  }

  pal <- palette_uke(palette = palette, reverse = reverse)

  if (discrete) {
    ggplot2::discrete_scale(aesthetics = aesthetics, palette = pal, ...)
  } else {
    ggplot2::scale_color_gradientn(colours = pal(256), aesthetics = aesthetics, ...)
  }
}


# Aliases -----------------------------------------------------------------

#' @export
scale_colour_uke <- scale_color_uke

# Fill --------------------------------------------------------------------


#' @rdname scale_color_uke
#' @export
scale_fill_uke <- function(palette = NULL,
                           discrete = TRUE,
                           reverse = FALSE,
                           aesthetics = "fill",
                           ...) {
  if (is.null(palette)) {
    palette <- ifelse(discrete, "uke", "gradient")
  }
  pal <- palette_uke(palette = palette, reverse = reverse)

  if (discrete) {
    ggplot2::discrete_scale(aesthetics = aesthetics, palette = pal, ...)
  } else {
    ggplot2::scale_fill_gradientn(colours = pal(256), aesthetics = aesthetics, ...)
  }
}


# Palette --------------------------------------------------------------------


uke_colors_list <- c(
  `blue` = "#00529A",
  `light blue` = "#E3E5F2",
  `brown` = "#9E9084",
  `light brown` = "#E9E4E1",
  `dark grey` = "#58595B",
  `light grey` = "#E6E7E8",
  `red` = "#B12833",
  `light red` = "#F4E6E1",
  `green` = "#6BC399",
  `light green` = "#DFF0E6",
  `yellow` = "#FFDF00",
  `light yellow` = "#FFF3BE",
  `orange` = "#F58021",
  `light orange` = "#FEE8D4",
  `blue grey` = "#6C90A7",
  `light blue grey` = "#E7EAEE",
  `purple` = "#B093BF",
  `light purple` = "#EFEAF3",
  `cyan` = "#5BC6CC",
  `light cyan` = "#DEF1F1"
)


#' @export
uke_colors <- function(...) {
  cols <- c(...)

  if (is.null(cols)) {
    return(uke_colors_list)
  }

  uke_colors_list[cols]
}


uke_palettes <- list(
  `uke` = uke_colors("blue", "cyan", "orange", "blue grey", "red"),
  `full` = uke_colors(),
  `ice` = uke_colors("cyan", "blue grey", "light green", "light blue"),
  `contrast` = uke_colors("blue", "orange", "yellow", "green", "red"),
  `complement` = uke_colors("blue", "light blue", "green", "light green", "yellow", "light yellow", "red", "light red"),
  `light` = uke_colors("light blue", "light cyan", "light orange", "light blue grey", "light red"),
  `dark` = uke_colors("blue", "brown", "dark grey", "red", "green", "yellow", "orange", "blue grey", "purple", "cyan"),
  `gradient` = uke_colors("blue", "blue grey", "purple", "red", "orange", "yellow"),
  `light gradient` = uke_colors("light blue", "light blue grey", "light purple", "light red", "light orange", "light yellow")
)


#' @export
palette_uke <- function(palette = "uke", reverse = FALSE, ...) {
  .retrieve_palette(palette, uke_palettes, reverse = reverse, ...)
}


# helper -----------------------

.retrieve_palette <- function(palette, palette_list, reverse = FALSE, ...) {
  if (!palette %in% names(palette_list)) {
    msg <- c(paste0(
      "Palette name not available. `palette` must be one of ",
      datawizard::text_concatenate(names(palette_list), last = " or ", enclose = "`"),
      "."
    ), "Using default palette now.")
    insight::format_warning(msg)
    palette <- 1
  }
  pal <- palette_list[[palette]]

  if (reverse) pal <- rev(pal)

  grDevices::colorRampPalette(pal, ...)
}
