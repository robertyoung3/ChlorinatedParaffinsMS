#' customize_col_names
#'
#' @param MSdata a list of tibbles containing MSdata for the SCCP congeners
#'   (written for total_SCCPs)
#'
#' @return a list of tibbles with customized column names
#' @export
customize_col_names <- function(MSdata = total_SCCPs) {
  for (i in seq_along(MSdata)) {
    if (names(MSdata[i]) %in% c("arkiv", "cond", "hydromatrix", "mdblank",
                              "sample", "tobis")) {
      MSdata[[i]] <- MSdata[[i]] %>%
        dplyr::rename(replicate = nominal_perc_Cl,
                      CF = concentration) %>%
        janitor::remove_empty("cols")
    }

    if (names(MSdata[i]) == "cond") {
      MSdata[[i]] <- MSdata[[i]] %>%
        dplyr::rename(inj_num = replicate)
    }

    if (names(MSdata[i]) == "sample") {
      MSdata[[i]] <- MSdata[[i]] %>%
        dplyr::rename(sample_name = replicate) %>%
        dplyr::mutate(sample_name = as.character(sample_name))
    }

    temp_names_replicate <- c("injection", "replicate")
    if (names(MSdata[i]) == "replicate") {
      MSdata[[i]] <- MSdata[[i]] %>%
        tidyr::separate(col = concentration, into = temp_names_replicate, sep = "\\.")
    }
  }
  return(MSdata)
}
