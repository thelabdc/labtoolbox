#' @title Generate Reproducible Random Seed
#'
#' @description Generate an integer seed for reproducible randomization or simulation
#'
#' @details Two seed types are supported:
#' \itemize{
#'   \item \code{"date"}: constructed from the current local time in the format \code{yymmddHH}.
#'   \item \code{"sampled"}: sampled uniformly from the integers \code{1} to \code{1e9}.
#' }
#' For either type, run this function once and record the resulting seed in your code.
#'
#' @param seed_type seed type: \code{"date"} (default) or \code{"sampled"}
#'
#' @return An integer scalar seed
#'
#' @examples
#' # Generate a seed based on the current date/time and use it
#' set.seed(gen_lab_seed())
#' runif(3)
#'
#' # Generate a sampled seed and use it
#' set.seed(gen_lab_seed("sampled"))
#' rnorm(3)
#'
#' @export
gen_lab_seed <- function(seed_type = c("date", "sampled")) {
  seed_type <- match.arg(seed_type)
  
  if (seed_type == "date") {
    # yymmddHH; fits within 32-bit integer range
    return(as.integer(format(Sys.time(), "%y%m%d%H")))
  } else {
    # sampled uniformly from 1..1e9
    return(as.integer(sample.int(1e9, 1)))
  }
}
