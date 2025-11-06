#' Sample Business License Data
#'
#' A sample of raw business license data pulled from Open Data
#'
#'
#' @format ## `sample_bbl`
#' A data frame with 10,000 rows and 44 columns. The data is a random sample taken from data downloaded from OpenDataDC on August 25, 2025.
#' @source <https://opendata.dc.gov/datasets/85bf98d3915f412c8a4de706f2d13513_0/explore>
"sample_bbl"

#https://maps2.dcgis.dc.gov/dcgis/rest/services/DCGIS_DATA

#' Download spatial data from Open Data DC
#'
#'Download and read in spatial data from a feature service on Open Data DC.
#'You can find a directory of spatial data at \url{https://maps2.dcgis.dc.gov/dcgis/rest/services/DCGIS_DATA}.

#' @param convert_crs By default, the package reads in the shapefile in WGS 84 (ESPG 4326). If you plan to use the data for distance calculations, setting convert_crs = TRUE will convert the shapefile to DC GIS's preferred projection: The Maryland State Plane coordinate system of the American Datum of 1983 (NAD 83).
#' @param print_map Prints a simple shapefile of the data being read in as a side product.
#'
#' @return An sf object
#' @export

read_sf_opendatadc <- function(url = "https://maps2.dcgis.dc.gov/dcgis/rest/services/DCGIS_DATA/Administrative_Other_Boundaries_WebMercator/MapServer/10",
                            convert_crs = FALSE,
                            print_map = TRUE){
url_query <- paste(url, "/query?outFields=*&where=1%3D1&f=geojson", sep = "")
geojson <- sf::st_read(url_query, quiet = TRUE)
if(convert_crs==FALSE){
  shapefile <- sf::st_as_sf(geojson, crs=4326)
}
if(convert_crs==TRUE){
  shapefile <- sf::st_as_sf(geojson, crs=26985) # Convert to Maryland State Plane coordinate system of the American Datum of 1983 (NAD 83).
}
 p <-  ggplot(shapefile) + geom_sf()+ theme_classic()
 if(print_map==TRUE){
 print(p)
   }
return(shapefile)
}
