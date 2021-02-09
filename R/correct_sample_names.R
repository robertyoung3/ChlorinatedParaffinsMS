#' correct_sample_names
#'
#' @param MSdata a tibble of MS data with incorrect sample names
#'
#' @return a corrected tibble
#' @export
correct_sample_names <- function(MSdata) {
  temp <- MSdata$sample %>%
    dplyr::select(batch, filename)
  temp_names <- c("year", "batch", "name")
  temp$filename <- temp$filename %>%
    stringr::str_replace(pattern = "_rep", replacement = ".")
  temp <- temp %>%
    tidyr::separate(col = filename, into = temp_names, sep = "_") %>%
    dplyr::select(name)
  MSdata$sample$num1 <- temp$name
  return(MSdata)
}
