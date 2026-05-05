#' Create a basemap using DC's OpenData raster tiles
#'
#' @param bbox Named numeric vector (xmin, ymin, xmax, ymax) in WGS84 (EPSG:4326).
#'   Defaults to the DC extent. If NULL, derived from the outline layer.
#' @param base_source Outline layer: one of "all_dc", "ward", "anc", or "custom".
#' @param ward Optional filter for Ward IDs when base_source == "ward".
#' @param anc Optional filter for ANC IDs when base_source == "anc".
#' @param outline_url If base_source == "custom", a URL `sf::st_read()` can read (e.g., ArcGIS GeoJSON).
#' @param raster_source Raster endpoint: "dc_basemap" or "custom".
#' @param raster_url If raster_source == "custom", the Export Map endpoint URL.
#' @param img_path File path to write the PNG when write = TRUE.
#' @param width_px Target image width in pixels (height computed from aspect).
#' @param transparent Request transparency from the server (if supported).
#' @param write Logical; when FALSE, keeps the image in memory (no persistent file).
#' @return A ggplot2 plot containing the basemap (raster) and the outline.
#' @import sf httr png ggplot2 dplyr
#' @export
plot_dc_basemap <- function(
   # bbox = c(xmin = -77.11981, ymin = 38.79157, xmax = -76.90917, ymax = 38.99596),
  bbox = NULL,
    base_source  = c("all_dc", "ward", "anc", "custom"),
    ward = NULL,
    anc  = NULL,
    outline_url = NULL,
    raster_source = c("dc_basemap", "custom"),
    raster_url   = NULL,
    img_path     = "dc_basemap_citywide.png",
    width_px     = 2400,
    transparent  = TRUE,
    write        = TRUE
) {
  # ---- 1) Validate args ----
  base_source   <- match.arg(base_source)
  raster_source <- match.arg(raster_source)
  
  if (!is.null(ward) && !is.null(anc)) {
    stop("Please supply either `ward` or `anc`, not both.")
  }
  
  if (!is.null(bbox)) {
    if (!is.numeric(bbox) || length(bbox) != 4) {
      stop("`bbox` must be a numeric vector of length 4 (xmin, ymin, xmax, ymax) in EPSG:4326.")
    }
    if (is.null(names(bbox)) || !all(c("xmin","ymin","xmax","ymax") %in% names(bbox))) {
      names(bbox) <- c("xmin","ymin","xmax","ymax")
    }
  }
  
  # ---- 2) Read outline layer ----
  url_all_dc <- "https://maps2.dcgis.dc.gov/DCGIS/rest/services/DCGIS_DATA/DC_Basemap_WebMercator/MapServer/0/query?outFields=*&where=1%3D1&f=geojson"
  url_ward   <- "https://maps2.dcgis.dc.gov/dcgis/rest/services/DCGIS_DATA/Administrative_Other_Boundaries_WebMercator/MapServer/53/query?outFields=*&where=1%3D1&f=geojson"
  url_anc    <- "https://maps2.dcgis.dc.gov/dcgis/rest/services/DCGIS_DATA/Administrative_Other_Boundaries_WebMercator/MapServer/54/query?outFields=*&where=1%3D1&f=geojson"
  
  outline_url_final <- switch(
    base_source,
    "all_dc" = url_all_dc,
    "ward"   = url_ward,
    "anc"    = url_anc,
    "custom" = {
      if (is.null(outline_url)) stop("When base_source == 'custom', please supply `outline_url`.")
      outline_url
    }
  )
  
  outline <- sf::st_read(outline_url_final, quiet = TRUE)
  
  # ---- 3) Optional filters for ward or ANC ----
  # Filter to ward
  if (!is.null(ward) && base_source == "ward") {
        outline <- dplyr::filter(outline, WARD == ward)
    }

  # Filter to ANC
  if (!is.null(anc) && base_source == "anc") {
    outline <- dplyr::filter(outline, ANC_ID == anc)
  }

  
  # ---- 4) Derive bbox from outline if not provided ----
  #if (is.null(bbox)) {
  #  outline_4326 <- sf::st_transform(outline, 4326)
  #  bbox <- sf::st_bbox(outline_4326)
    #bbox <- c(bb["xmin"], bb["ymin"], bb["xmax"], bb["ymax"])
  #}

  
  if (is.null(bbox)) {
    outline_4326 <- sf::st_transform(outline, 4326)
    bbox <- sf::st_bbox(outline_4326)
  }
  
  
  #bbox <- st_bbox(outline)
  
  
  # ---- 5) Fetch raster via helper (write or in-memory) ----
  fr <- fetch_dc_raster(
    bbox          = bbox,
    raster_source = raster_source,
    raster_url    = raster_url,
    width_px      = width_px,
    transparent   = transparent,
    write         = write,
    img_path      = img_path
  )
  
  # ---- 6) Read PNG (from path or raw) ----
  if (write) {
    img <- png::readPNG(fr$path)
  } else {
    con <- rawConnection(fr$raw, open = "rb")
    on.exit(close(con), add = TRUE)
    img <- png::readPNG(con)
  }
  
  # ---- 7) Plot raster + outline ----
  outline_4326 <- sf::st_transform(outline, 4326)
  
  p <- ggplot2::ggplot() +
    ggplot2::annotation_raster(
      raster = img,
      xmin = bbox["xmin"], xmax = bbox["xmax"],
      ymin = bbox["ymin"], ymax = bbox["ymax"]
    ) +
    ggplot2::geom_sf(data = outline_4326, fill = NA, color = "black", linewidth = 0.5) +
    ggplot2::coord_sf(
      xlim = c(bbox["xmin"], bbox["xmax"]),
      ylim = c(bbox["ymin"], bbox["ymax"]),
      expand = FALSE
    ) +
    ggplot2::theme_void()
  
  return(p)
}