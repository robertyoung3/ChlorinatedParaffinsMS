#' plot_calib_zero_intercept
#'
#' @param MSdata a tibble containing response and concentration of a technical
#'   mixture or surrogate standard
#' @param model a list containing output from the lm function
#' @param type a character string indicating whether a "linear", "quadratic" or
#'   "exponential" model was used
#'
#' @return plot as side effect
#' @export
plot_calib_zero_intercept <- function(MSdata, model, type = "exponential") {
  # common variables
  b_x <- signif(coef(model)[[1]], 3)
  RMSE <- signif(sqrt(mean(resid(model)^2)), 4)
  RMSE_label <- bquote(italic(RMSE) == .(RMSE))
  xrange <- range(MSdata$concentration, na.rm = TRUE)
  yrange <- range(MSdata$total_congener_response, na.rm = TRUE)

  # common plotting information
  if(is.character(MSdata$nominal_perc_Cl)) {
    plot_title <- MSdata$compound[1]
    conc_units <- "ng/mL"
  } else {
    plot_title <- paste(MSdata$nominal_perc_Cl[1], "% Cl")
    conc_units <- expression(mu*"g SCCP/mL")
  }

  # common plot
  calib_plot <- function(data, mapping, method, formula, ss_args = NULL) {
    ggplot2::ggplot(data, mapping) +
      ggplot2::geom_point(size = 2, shape = 15, color = "red") +
      ggplot2::stat_smooth(method = method, formula = formula, method.args = ss_args,
                           se = FALSE, color = "black") +
      ggplot2::ggtitle(plot_title) +
      ggplot2::xlab(conc_units) +
      ggplot2::ylab("Response") +
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
    y_eq_label <- bquote(italic(y) == .(b_x)*italic(x))
    r_sq <- signif(summary(model)$adj.r.squared, 4)
    r_sq_label <- bquote(italic(R)^2 == .(r_sq))

    calib_plot(data = MSdata,
               ggplot2::aes(x = concentration, y = total_congener_response),
               method = "lm",
               formula = y ~ x - 1)

  } else if (type == "quadratic") {
    c_x_sq <- signif(coef(model)[[2]], 3)
    y_eq_label <- bquote(italic(y) == .(b_x)*italic(x) + .(c_x_sq)*italic(x)^2)
    r_sq <- signif(summary(model)$adj.r.squared, 4)
    r_sq_label <- bquote(italic(R)^2 == .(r_sq))

    calib_plot(data = MSdata,
               ggplot2::aes(x = concentration, y = total_congener_response),
               method = "lm",
               formula = y ~ x + I(x^2) - 1)

  } else if (!type %in% c("linear", "quadratic")) {
    stop("The regression model must be \"linear\" or  \"quadratic\"")
  }
}
