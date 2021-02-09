#' get_surrog_calib
#'
#' @param MSdata a tibble containing the surrogate data from the calib standards
#'
#' @return a tibble containing the surrogate calib standards
#' @export
get_surrog_calib <- function(MSdata) {
#  temp_names <- c("num3", "num4", "surrog_conc")
  MSdata %>%
#    tidyr::separate(col = sample_id, into = temp_names, sep = "_") %>%
    dplyr::rename(surrog_conc = spike_conc) %>%
    dplyr::mutate(surrog_conc = as.numeric(surrog_conc)) %>%
    dplyr::filter(nominal_perc_Cl == 51.5, peak_label == "T1")
#    dplyr::select(-num3, -num4)
}
