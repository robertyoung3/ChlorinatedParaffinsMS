#' plot_isotopic_pattern
#'
#' @param chem_formula a character string containing a molecular formula
#'   reference to the enhanced SCCP isotope list
#'
#' @return produces a plot of isotopic abundances
#' @export
#'
#' @examples
#' chem_formula <- "C10H15Cl7"
#' plot_isotopic_pattern(chem_formula)
plot_isotopic_pattern <- function(chem_formula) {
  spectral_data <- tibble::as_tibble(SCCP_isotope_list[[chem_formula]])
  xrange_low <- round(range(spectral_data$`m/z`)[1] - 1)
  xrange_high <- round(range(spectral_data$`m/z`)[2] + 1)
  sample_title <- chem_formula
  ggplot2::ggplot(data = spectral_data,
                  ggplot2::aes(x = `m/z`, ymin = 0, ymax = rel_isotop_abund)) +
    ggplot2::geom_linerange(size = 0.1) +
    ggplot2::ggtitle(sample_title) +
    ggplot2::xlab("m/z") +
    ggplot2::ylab("Isotopic Abund.") +
    ggplot2::scale_x_continuous(limits = c(xrange_low, xrange_high), breaks = seq(xrange_low, xrange_high, by = 2)) +
    ggplot2::scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 25)) +
    ggthemes::theme_tufte(base_size = 18, base_family = "sans") +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
}
