#' plot_RF_model
#'
#' @param MSdata a tibble containing response factor and \%Cl data for a range of
#'   SCCP technical mixture standards
#' @param regr a list containing output from the lm function
#' @param type a character string indicating whether a "linear" or "quadratic"
#'   model was used
#' @param colorBy a character string indicating whether to color by "TMstd"
#'   (designated by its nominal \% Cl) or "concentration".
#'
#' @return plot as side effect
#' @export
plot_RF_model <- function(MSdata, regr, type = "linear", colorBy = "TMstd") {
  # common variables
  a_intercept <- signif(coef(regr)[[1]], 3)
  b_x <- signif(coef(regr)[[2]], 3)
  RMSE <- signif(sqrt(mean(resid(regr)^2)), 4)
  RMSE_label <- bquote(italic(RMSE) == .(RMSE))
  xrange <- range(MSdata$experimental_perc_Cl)
  yrange <- range(MSdata$experimental_RF)
#  temp_xrange <- range(total_SCCPs$calib$experimental_perc_Cl)
#  temp_yrange <- range(total_SCCPs$calib$experimental_RF)
#  xrange <- c(59.5, 65.5)
#  yrange <- c(0, 2.1)

  if (colorBy == "TMstd") {
    color <- dplyr::quo(nominal_perc_Cl)
    legend_title <- "Nominal %Cl"
  } else if (colorBy == "concentration") {
    color = dplyr::quo(concentration)
    legend_title <- "ug/mL"
  } else if (!colorBy %in% c("TMstd", "concentration")) {
    stop("The data points can only be colored by \"TMstd\" (nominal wt% Cl) or \"concentration\"")
  }

  # common plotting information
  plot_title <- bquote(Sigma~"SCCP")

  # common plot
  calib_plot <- function(data, mapping, method, formula, ss_args = NULL) {
    ggplot2::ggplot(data, mapping) +
      ggplot2::geom_point(size = 2, shape = 15) +
      ggplot2::stat_smooth(method = method, formula = formula, method.args = ss_args,
                           se = FALSE, color = "black") +
      ggplot2::ggtitle(plot_title) +
#      ggplot2::scale_x_continuous(limits = xrange, breaks = seq(xrange[1], xrange[2], by = (xrange[2] - xrange[1])/6)) +
#      ggplot2::scale_y_continuous(limits = yrange, breaks = seq(yrange[1], yrange[2], by = yrange[2]/3)) +
      ggplot2::xlab("Experimental %Cl") +
      ggplot2::ylab("Response Factor") +
      ggplot2::labs(col = legend_title) +
      ggsci::scale_color_npg() +
      ggthemes::theme_tufte(base_size = 11, base_family = "sans") +
      ggplot2::theme(plot.title = ggplot2::element_text(size = 14, face = "bold")) +
      ggplot2::annotate("text",
                        x = xrange[1] + 0.1*(xrange[2] - xrange[1]),
                        y = c(yrange[2] - 0.1*(yrange[2] - yrange[1]),
                              yrange[2] - 0.2*(yrange[2] - yrange[1]),
                              yrange[2] - 0.3*(yrange[2] - yrange[1])),
                        size = 3.5, hjust = 0,
                        label = c(y_eq_label, RMSE_label, r_sq_label),
                        parse = TRUE)
  }

  if (type == "linear") {
    y_eq_label <- bquote(italic(y) == .(a_intercept) + .(b_x)*italic(x))
    r_sq <- signif(summary(regr)$adj.r.squared, 4)
    r_sq_label <- bquote(italic(R)^2 == .(r_sq))

    calib_plot(data = MSdata,
               ggplot2::aes(x = experimental_perc_Cl,
                            y = experimental_RF,
                            color = factor(!!color)),
               method = "lm",
               formula = y ~ x)

  } else if (type == "quadratic") {
    c_x_sq <- signif(coef(regr)[[3]], 3)
    y_eq_label <- bquote(italic(y) == .(a_intercept) + .(b_x)*italic(x) + .(c_x_sq)*italic(x)^2)
    r_sq <- signif(summary(regr)$adj.r.squared, 4)
    r_sq_label <- bquote(italic(R)^2 == .(r_sq))

    calib_plot(data = MSdata,
               ggplot2::aes(x = experimental_perc_Cl,
                            y = experimental_RF,
                            color = factor(!!color)),
               method = "lm",
               formula = y ~ poly(x, degree = 2))

  } else if (type == "exponential") {
    y_eq_label <- bquote(y == .(a_intercept)*e^{.(b_x)*(x-x[min])})
    r_sq_label <- bquote(italic(R)^2 == ~"N/A")
    start_a <- min(MSdata$experimental_RF)
    start_b <- 0.2   # estimated rate of increase

    calib_plot(data = MSdata,
               ggplot2::aes(x = experimental_perc_Cl,
                            y = experimental_RF,
                            color = factor(!!color)),
               method = "nls",
               formula = y ~ (a * exp(b * (x - min(x)))),
               ss_args = list(start = list(a = start_a, b = start_b)))  # fails with a = min(y)

  } else if (!type %in% c("linear", "quadratic", "exponential")) {
    stop("The regression model must be \"linear\",  \"quadratic\", or \"exponential\"")
  }
}
