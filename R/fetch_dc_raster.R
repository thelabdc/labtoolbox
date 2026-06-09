#' Fetch a DC basemap raster image (PNG) for a given bbox
#'
#' @param bbox Named numeric vector (xmin, ymin, xmax, ymax) in WGS84 (EPSG:4326).
#' @param raster_source Which raster endpoint to use: "dc_basemap" or "custom".
#' @param raster_url If raster_source == "custom", the Export Map (or image) endpoint URL.
#' @param width_px Output image width in pixels (height computed from bbox aspect in EPSG:3857).
#' @param transparent Logical, request transparency (if the server supports it).
#' @param write Logical, write PNG to disk. If FALSE, returns raw bytes and avoids persistent I/O.
#' @param img_path Path to write the PNG when write = TRUE.
#' @param timeout Request timeout in seconds.
#' @return A list:
#'   - if write = TRUE: list(path, width_px, height_px, bbox_3857, params)
#'   - if write = FALSE: list(raw, width_px, height_px, bbox_3857, params)
#' @import sf httr
#' @export
fetch_dc_raster <- function(
    bbox = c(xmin = -77.11981, ymin = 38.79157, xmax = -76.90917, ymax = 38.99596),
    raster_source = c("dc_basemap", "custom"),
    raster_url   = NULL,
    width_px     = 2400,
    transparent  = TRUE,
    write        = TRUE,
    img_path     = "dc_basemap_citywide.png",
    timeout      = 30
) {
  raster_source <- match.arg(raster_source)
  
  # Validate bbox
  if (is.null(bbox) || !is.numeric(bbox) || length(bbox) != 4) {
    stop("`bbox` must be a numeric vector of length 4 (xmin, ymin, xmax, ymax) in EPSG:4326.")
  }
  if (is.null(names(bbox)) || !all(c("xmin","ymin","xmax","ymax") %in% names(bbox))) {
    names(bbox) <- c("xmin","ymin","xmax","ymax")
  }
  
  # Transform bbox to EPSG:3857 for the export service
  bbox_sfc_4326 <- sf::st_as_sfc(sf::st_bbox(bbox, crs = sf::st_crs(4326)))
  bbox_3857     <- sf::st_bbox(sf::st_transform(bbox_sfc_4326, 3857))
  
  width_m  <- as.numeric(bbox_3857["xmax"] - bbox_3857["xmin"])
  height_m <- as.numeric(bbox_3857["ymax"] - bbox_3857["ymin"])
  height_px <- max(1L, round(width_px * height_m / width_m))
  
  # Default DC Export Map endpoint
  default_export_url <- "https://maps2.dcgis.dc.gov/dcgis/rest/services/DCGIS_DATA/DC_Basemap_WebMercator/MapServer/export"
  
  export_url <- switch(
    raster_source,
    "dc_basemap" = default_export_url,
    "custom"     = {
      if (is.null(raster_url)) stop("When raster_source == 'custom', please supply `raster_url`.")
      raster_url
    }
  )
  
  params <- list(
    bbox        = paste(bbox_3857["xmin"], bbox_3857["ymin"], bbox_3857["xmax"], bbox_3857["ymax"], sep = ","),
    bboxSR      = 3857,
    size        = paste(width_px, height_px, sep = ","),
    format      = "png32",
    transparent = if (transparent) "true" else "false",
    f           = "image"
  )
  
  resp <- httr::GET(export_url, query = params, httr::timeout(timeout))
  httr::stop_for_status(resp)
  img_raw <- httr::content(resp, "raw")
  
  if (write) {
    writeBin(img_raw, img_path)
    return(list(
      path       = img_path,
      width_px   = width_px,
      height_px  = height_px,
      bbox_3857  = bbox_3857,
      params     = params
    ))
  } else {
    return(list(
      raw        = img_raw,
      width_px   = width_px,
      height_px  = height_px,
      bbox_3857  = bbox_3857,
      params     = params
    ))
  }
}