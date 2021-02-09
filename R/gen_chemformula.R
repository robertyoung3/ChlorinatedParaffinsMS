#' gen_chemformula
#'
#' @param elements_df a data.frame containing elements as variables and element
#'   counts as values.
#'
#' @import rebus.base
#'
#' @return a character string containing the corresponding chemical formula
#' @export
#'
#' @examples
#' elements_df <- tibble::tibble(C = 17, H = 22, Br = 1, F = 13, O = NA, S = 0)
#' gen_chemformula(elements_df)
gen_chemformula <- function(elements_df) {
  elements_df <- elements_df %>%
    dplyr::select_if(~!is.na(.))
  element_list <- get_element_list(elements_df)
  # build character string from elements_df
  chemformula <- ""
  for (i in 1:length(element_list)) {
    chemformula <- stringr::str_c(chemformula, element_list[i], elements_df[element_list[i]])
}
  # remove elements with zero counts
  chemformula <- stringr::str_replace_all(chemformula,
                                          pattern = repeated(ALPHA, 1,2) %R% "0",
                                          replacement = "")
  # remove #1 for one digit element counts = 1 within string
  chemformula <- stringr::str_replace_all(chemformula,
                                          pattern = capture(repeated(ALPHA, 1, 2)) %R%
                                            "1" %R% capture(ALPHA),
                                          replacement = stringr::str_c(REF1, REF2))
  # remove 1 for elements with one count (but not >= 10 counts) at string end
  chemformula <- stringr::str_replace(chemformula,
                                      pattern = capture(repeated(ALPHA, 1, 2)) %R%
                                        "1" %R% END,
                                      replacement = REF1)
  return(chemformula)
}
