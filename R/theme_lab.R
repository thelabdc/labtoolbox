#' Lab @ DC ggplot theme
#'
#' @param ppt Logical; format for PowerPoint (TRUE) or standard report (FALSE).
#' @return A ggplot2 theme object.
#' @import ggplot2
#' @export

theme_lab <- function(ppt = FALSE) {
  
  # Default font can be overridden by package users
  base_font <- getOption("lab.base_font", default = "Neutra")
  
  if (!ppt) {
    base_size_title <- 55
    base_size_subtitle <- 50
    base_size_cap <- 40
    base_size_axis_title <- 40
    base_size_axis <- 35
    base_size_legend <- 40
    theme_font <- NULL
  } else {
    base_size_title <- 60
    base_size_subtitle <- 55
    base_size_cap <- 40
    base_size_axis_title <- 45
    base_size_axis <- 40
    base_size_legend <- 45
    theme_font <- base_font
  }
  
  thm <- theme_void(base_family = theme_font) +
    theme(
      plot.title = element_text(size = base_size_title, hjust = 0.5, lineheight = 0.4),
      plot.subtitle = element_text(size = base_size_subtitle),
      plot.caption = element_text(size = base_size_cap, hjust = 1, lineheight = 0.4),
      
      axis.title = element_text(size = base_size_axis_title, lineheight = 0.4),
      axis.text  = element_text(size = base_size_axis, lineheight = 0.4),
      
      axis.title.y = element_text(angle = 90, margin = margin(r = 15)),
      axis.title.x = element_text(margin = margin(b = 5)),
      
      legend.title = element_text(size = base_size_legend, lineheight = 0.25),
      legend.text  = element_text(size = base_size_legend, lineheight = 0.4),
      legend.position = "bottom"
    )
  
  return(thm)
}
