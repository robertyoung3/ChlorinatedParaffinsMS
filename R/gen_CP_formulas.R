#' gen_CP_formulas
#'
#' Produces a dataframe with the elemental composition of chlorinated paraffin
#' formulas with the specified number of carbons and chlorines.
#'
#' @param C_num an integer or range of integers representing the number of
#'   carbons in a chlorinated paraffin formula
#' @param Cl_num an integer or range of integers representing the number of
#'   chlorines in a chlorinated paraffin formula
#'
#' @return a vector of character strings
#' @export
#'
#' @examples
#' C_num <- 10:13
#' Cl_num <- 5:10
#' gen_CP_formulas(C_num, Cl_num)
gen_CP_formulas <- function (C_num, Cl_num) {
  #generable table of elements
  temp_file <- tibble::data_frame(C = integer(),
                                  H = integer(),
                                  Cl = integer())
  # Cn H2n+2−x Clx
  # SCCP: 10:13, MCCP: 14-17, LCCP: >17
    for(i in 1:length(C_num)) {
    for(j in 1:length(Cl_num)) {
      temp_df <- c(C = C_num[i],
                   H = 2*C_num[i] + 2 - Cl_num[j],
                   Cl = Cl_num[j])
      temp_file <- dplyr::bind_rows(temp_file, temp_df)
    }
  }
  #generate chemical formulas
  temp <- character()
  for(i in 1:nrow(temp_file)) {
    temp[i] <- gen_chemformula(temp_file[i, ])
  }
  return(temp)
}
