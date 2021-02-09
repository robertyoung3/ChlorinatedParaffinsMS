#' split_MSdata
#'
#' @param MSdata a tibble containing the imported TraceFinder data, and
#'   containing a column which specifies the user-defined sample type
#'
#' @return a list of tibbles, separated by sample type
#' @export
split_MSdata <- function(MSdata) {
  split(x = MSdata, f = as.factor(MSdata$type))
}
