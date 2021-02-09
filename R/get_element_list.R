#' get_element_list
#'
#' Receives a named dataframe containing columns of chemical element counts and
#' returns a vector of element names in Hill System Order (C, H, alphabetical).
#'
#' @param elements_df A dataframe with chemical elements as column names.
#'
#' @return A vector of chemical element names.
#' @export
#'
#' @examples
#' test_df <- data.frame(O = 3, F = 5, H = 1, C = 2, S = 1)
#' get_element_list(test_df)
get_element_list <- function(elements_df) {
  element_list <- colnames(elements_df)
  element_list <- sort(element_list)
  ## put C and H first, then alphabetical
  if (any(element_list == "C")) {temp_list <- "C"}
  if (any(element_list == "H")) {temp_list <- c(temp_list, "H")}
  element_list <- unique(c(temp_list, element_list))
  return(element_list)
}
