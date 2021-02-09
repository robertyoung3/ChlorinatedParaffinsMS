#' transpose_df
#'
#' @param df a tibble or dataframe to transpose (e.g., rows to columns)
#'
#' @return a transposed tibble or dataframe
#' @export
transpose_df <- function(df) {
  t_df <- data.table::transpose(df)
  colnames(t_df) <- rownames(df)
  rownames(t_df) <- colnames(df)
  t_df <- t_df %>%
    tibble::rownames_to_column() %>%
    tibble::as_data_frame()
  return(t_df)
}
