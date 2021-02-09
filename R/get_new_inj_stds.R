#' get_new_inj_stds
#'
#' @param MSdata a tibble containing MSdata from TraceFinder
#'
#' @return a tibble containing the surrogate data
#' @export
get_new_inj_stds <- function(MSdata) {
  MSdata %>%
    dplyr::select(batch, batch_order, filename, area) %>%
    dplyr::rename(inj_std_area = area)
}
