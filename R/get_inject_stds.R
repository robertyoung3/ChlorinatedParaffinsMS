#' get_inject_stds
#'
#' @param MSdata a tibble containing MSdata from TraceFinder
#' @param std a character string containing the injection standard name, as
#'   specified in the "compound" column. The default is the injection standard
#'   used during method development.
#'
#' @return a tibble containing the injection standard data
#' @export
get_inject_stds <- function(MSdata, std = "13C-HCB") {
  dplyr::filter(MSdata, compound == std)
}
