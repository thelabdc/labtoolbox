#' Custom ggplot2 theme for The Lab
#'
#' Applies a minimal theme and custom discrete color/fill scales.
#'
#' @param base_size A numeric base font size.
#' @param palette A character string; one of \code{"lab"}, \code{"ddot"}, or a character vector of hex colors.
#' @param fonts_mayor A logical. If \code{TRUE}, uses the Neutra Text font family
#'
#' @return A list of ggplot2 theme and scale components.
#' @export

theme_lab <- function(base_size = 12, palette = "lab", fonts_mayor = FALSE) {

  # Handle palette input
  if (is.character(palette) && length(palette) == 1) {
    palette <- switch(palette,
                      "lab" = lab_colors,
                      "ddot" = ddot_colors,
                      stop("Invalid palette name. Use 'lab', 'ddot', or provide a vector of hex codes."))
  }

  # Internal scale functions
  scale_color_lab <- function() {
    ggplot2::discrete_scale("colour", "lab_colors", palette = function(n) palette)
  }

  scale_fill_lab <- function() {
    ggplot2::discrete_scale("fill", "lab_fill", palette = function(n) palette)
  }

  # Set font family
  base_family <- if (fonts_mayor) "Neutra" else ""

  # Define the theme
  custom_theme <- ggplot2::theme_classic(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = base_size * 1.2, color = "black", hjust = .5),
      plot.subtitle = ggplot2::element_text(size = base_size, color = "#333333",hjust = .5),
      axis.title = ggplot2::element_text(size = base_size, color = "#333333"),
      axis.text = ggplot2::element_text(size = base_size * 0.9, color = "#555555"),
      panel.grid.major = ggplot2::element_line(color = "white"),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom"
    )

  # Return theme and scales as a list
  list(
    custom_theme,
    scale_color_lab(),
    scale_fill_lab()
  )
}
