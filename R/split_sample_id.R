#' split_sample_id
#'
#' @param MSdata a list of tibbles containing MSdata for the SCCP congeners
#'
#' @return a list of tibbles with customized column names from the "sample_id" column
#' @export
split_sample_id <- function(MSdata) {
  temp_names <- c("spike_type", "spike_id", "spike_conc")
  MSdata %>%
    tidyr::separate(col = sample_id, into = temp_names, sep = "_")
}
