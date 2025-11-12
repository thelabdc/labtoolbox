#' Geocode addresses using the MAR
#'
#' This function queries the DC Master Address Repository (\url{https://octo.dc.gov/service/master-address-repository}) and returns details about the location.
#' You will need to download a (free) MAR API key and input it to the function (remember not to hard-code the key itself if you are pushing your code to GitHub.)\cr \cr
#' Warning: The code takes a long time to run. We recommend running once and then saving the result.

#' @param address A character string with an address in Washington, DC
#' @param mar_api_key A character vector that identifies you to the MAR. Request a key at \url{https://developers.data.dc.gov/guide/getapikey}.
#' @param cols Selects which columns of the response you want to return. By default, returns latitude and longitude.
#' @param return_NA A logical indicating whether to return missing values
#' 
#' @return Prints a data frame with information about the requested address. If \code{NULL}, prints "No results found."
#' 
#' @examples
#' # For a single address
#' \dontrun{
#' geocode_address("955 L'Enfant Plaza", mar_api_key = mar_api_key, cols = c("Latitude", "Longitude"))
#' }
#'
#' # For batch geocoding
#' \dontrun{
#' sample_addresses |>
#'   mutate(geocode_result = map(address, ~ geocode_address_mar(.x, mar_api_key = mar_api_key))) |>
#'   unnest_wider(geocode_result)
#' }
#'
#' @export
#'

# Geocode addresses using the MAR
geocode_address_mar <- function(address = "955 L'Enfant Plaza SW",
                            mar_api_key,
                            cols = c("Latitude", "Longitude"),
                            return_NA = TRUE){
  
  # Helper function to replace NULLs with NA
  null_to_na <- function(x) {
    x[sapply(x, is.null)] <- NA
    x
  }

  # Clean addresses: The MAR cannot accept slashes in "1/2" addresses
  # Replace any "1/2" in the address fields with 1-2
  address <- stringr::str_replace_all(address, "1/2", "1-2")
  
  encoded_url <- utils::URLencode(address, reserve =TRUE)
  
  if(exists("mar_api_key")){
    url <- paste("https://datagate.dc.gov/mar/open/api/v2.2/locations/", encoded_url, "?apikey=", mar_api_key, sep = "") # original throws errors with the 1/2 addresses
    x <- httr::GET(url)
      json_data <- rjson::fromJSON(rawToChar(x$content))
      json_address <- json_data$Result$addresses
      if(is.null(json_address) == FALSE){
        address_df <- tibble::as_tibble(null_to_na(json_address[[1]]$address$properties)) |>
          dplyr::select(all_of(cols))
        return(address_df)
      }else{
        if(return_NA == FALSE){
       return("No results found")
          }
        else{
          return(
            as.data.frame(matrix(NA, nrow = 0, ncol = length(cols), dimnames = list(NULL, cols)))
          )
        }
      }
    }else{
    return("Please provide a MAR Api Key")
    }
}
