.lab_env <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  font_dir <- system.file("fonts", package = pkgname)
  sysfonts::font_add("Neutra",
                     regular = file.path(font_dir, "neutra-text.otf"),
                     bold = file.path(font_dir, "neutra-text-bold.otf"),
                     italic = file.path(font_dir, "neutra-text-bold-italic.otf"))
  showtext::showtext_auto()
}
