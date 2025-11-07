#' Lab color palette
#'
#' An unnamed vector of hex codes for the Lab colors.
#'
#' @format An unnamed character vector of hex color codes.
#' @examples
#' lab_colors["lab_blue"]
#' scale_color_manual(values = lab_colors[c("lab_blue", "lab_red", "lab_light_blue")])
#'
#' @export
lab_colors <- c( "#4D4D4D", "#2B4888","#CF3A4B","#DCF1FE", "#1B998B", "#FFECC0","#FF6E52", "#012C3B")

#' Lab color palette
#'
#' A named vector of hex codes for the Lab colors.
#'
#' @format A named character vector with hex color codes.
#' @examples
#' lab_colors["lab_blue"]
#' scale_color_manual(values = lab_colors[c("lab_blue", "lab_red", "lab_light_blue")])
#'
#' @export
lab_colors_named <-
  c("lab_gray"="#4D4D4D",
    "lab_blue"="#2B4888",
    "lab_red"="#CF3A4B",
    "lab_teal"= "#1B998B",
    "lab_yellow"= "#FFECC0",
    "lab_coral" = "#FF6E52",
    "lab_light_blue" = "#DCF1FE",
    "lab_gunmetal" ="#012C3B")

#' DDOT color palette
#'
#' An unnamed vector of hex codes for the DDOT colors.
#'
#' @format A named character vector with hex color codes.
#' @examples
#' lab_colors["ddot_blue"]
#' scale_color_manual(values = ddot_colors[c("ddot_gray", "ddot_blue", "ddot_red")])
#'
#' @export
ddot_colors <- c("#012C3B","#2B4888","#CF3A4B")
#ddot_colors <- c("#FF6E52","#2B4888","#CF3A4B")


#' DDOT color palette
#'
#' A named vector of hex codes for the DDOT colors.
#'
#' @format A named character vector with hex color codes.
#' @examples
#' lab_colors["ddot_blue"]
#' scale_color_manual(values = ddot_colors[c("ddot_gray", "ddot_blue", "ddot_red")])
#'
#' @export
ddot_colors_named <- c("ddot_gray" ="#012C3B",
                 "ddot_blue"="#2B4888",
                 "ddot_red"="#CF3A4B"
)
