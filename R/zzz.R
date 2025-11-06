.lab_env <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  # Mayor's Font
  sysfonts::font_add(
    family = "Neutra",
    regular = system.file("fonts", "neutra-text.otf", package = "labtoolbox"),
    bold = system.file("fonts", "neutra-text-bold.otf", package = "labtoolbox"),
    italic = system.file("fonts", "neutra-text-alt.otf", package = "labtoolbox"),
    bolditalic = system.file("fonts", "neutra-text-bold-italic.otf", package = "labtoolbox"))
  showtext::showtext_auto()



}
