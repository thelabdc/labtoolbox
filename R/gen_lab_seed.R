#' Generate Lab Random Seed
#'
#' Generate a seed for replicable work involving randomization or simulation. At The Lab, we use two types of seeds:
#' \itemize{
#' \item date seeds (public-relevant implementations)
#' \item sampled seeds (all other implementations)
#' }
#' We consider *public-relevant* implementations to include situations like a random assignment of a program to some members of a waitlist, a random selection of some households to participate in a survey, and random assignment of hypothetical treatments during confirmatory randomization inference.\cr \cr

#' For either type of seed, run this function only once, and then include the seed in your code.

#' @param seed_type A character string specifying \code{"date"} or \code{"sampled"}; defaults to \code{"date"}
#'
#' @return Prints the generated seed
#' @examples
#' gen_lab_seed()
#' gen_lab_seed("sampled")
#' @export
#'
gen_lab_seed <- function(seed_type = "date") {
  #stopifnot(is.character(string), length(string) <=1)
  if(seed_type == "date"){
    return(
      format(Sys.time(), "%y%m%d%H")
      )
  }else{
    if(seed_type == "sampled"){
    return(sample(1e9, 1))
    }else{
    return(print("Please specify seed_type ('date' or 'sampled')"))
      }
  }
}
