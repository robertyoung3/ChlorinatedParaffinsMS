#'get_TFdata
#'
#'This function reads an Excel file exported by TraceFinder (one sample per
#'worksheet), cleans up the filenames, drops empty columns, converts numbers
#'occuring as strings, and returns a subset of the data when abbrev = TRUE.
#'
#'@param filename A character string containing the name of the Excel file that
#'  contains the TraceFinder MS data.
#'
#'@return a tibble containing MS data for the desired technical mixture
#'  standards.
#'
#'@export
get_TFdata <- function(filename) {
  sheetnames <- readxl::excel_sheets(filename)
  temp_names <- c("type", "num1", "num2")
  MSdata <- purrr::map_dfr(sheetnames, readxl::read_excel, path = filename,
                           na = c("N/A", "N/F", ""))
  # keeping `Sample ID` column, which may not contain sample information in every batch
  if(sum(!is.na(MSdata$`Sample ID`)) == 0) MSdata$`Sample ID`[[1]] <- "dummy"
  MSdata <- MSdata %>%
    janitor::remove_empty("cols") %>%
    janitor::clean_names("snake") %>%
    tidyr::separate(col = comments, into = temp_names, sep = "_") %>%
    dplyr::mutate_at(dplyr::vars(num1, num2), list(as.numeric)) %>%
    # TraceFinder directly reports the injection standard response only for the quantification ion
    dplyr::rename(TF_mz = m_z_expected) %>%
    dplyr::mutate(unit_TF_mz = round(TF_mz),
                  rel_response = area / istd_response) %>%
    dplyr::select(batch_order, filename, sample_id, type, num1, num2, compound, unit_TF_mz,
                  TF_mz, peak_label, area, rt, actual_rt, istd_response, rel_response)
  return(MSdata)
}
