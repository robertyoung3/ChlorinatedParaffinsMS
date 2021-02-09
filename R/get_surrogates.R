#' get_surrogates
#'
#' @param MSdata a tibble containing MSdata from TraceFinder
#' @param surrogate a character string containing the surrogate standard name,
#'   as specified in the "compound" column. The default is the surrogate
#'   standard used during method development.
#'
#' @return a tibble containing the surrogate data
#' @export
get_surrogates <- function(MSdata, surrogate = "13C-PCN27") {
  dplyr::filter(MSdata, compound == surrogate)
}
