#' @title autoplot method for multisimsum objects
#' @description `autoplot` can produce a series of plot to summarise results of simulation studies. See `vignette("C-plotting", package = "rsimsum")` for further details.
#' @param object An object of class `multisimsum`.
#' @param par The parameter results to plot.
#' @param type The type of the plot to be produced. Possible choices are: `forest`, `lolly`, `zip`, `est`, `se`, `est_ba`, `se_ba`, `est_density`, `se_density`, `est_hex`, `se_hex`, `est_ridge`, `se_ridge`, `heat`, `nlp`, with `forest` being the default.
#' @param stats Summary statistic to plot, defaults to `bias`. See [summary.simsum()] for further details on supported summary statistics.
#' @param target Target of summary statistic, e.g. 0 for `bias`. Defaults to `NULL`, in which case target will be inferred.
#' @param fitted Superimpose a fitted regression line, useful when `type` = (`est`, `se`, `est_ba`, `se_ba`, `est_density`, `se_density`, `est_hex`, `se_hex`). Defaults to `TRUE`.
#' @param scales Should scales be fixed (`fixed`, the default), free (`free`), or free in one dimension (`free_x`, `free_y`)?
#' @param top Should the legend for a nested loop plot be on the top side of the plot? Defaults to `TRUE`.
#' @param density.legend Should the legend for density and hexbin plots be included? Defaults to `TRUE`.
#' @param zoom A numeric value between 0 and 1 signalling that a zip plot should _zoom_ on the top x% of the plot (to ease interpretation). Defaults to 1, where the whole zip plot is displayed.
#' @param zip_ci_colours A string with (1) a hex code to use for plotting coverage probability and its Monte Carlo confidence intervals (the default, with value `zip_ci_colours = "yellow"`), (2) a string vector of two hex codes denoting optimal coverage (first element) and over/under coverage (second element) or (3) a vector of three hex codes denoting optimal coverage (first), undercoverage (second), and overcoverage (third).
#' @param ... Not used.
#'
#' @return A `ggplot` object.
#' @export
#'
#' @examples
#' data("frailty", package = "rsimsum")
#' ms <- multisimsum(
#'   data = frailty,
#'   par = "par", true = c(trt = -0.50, fv = 0.75),
#'   estvarname = "b", se = "se", methodvar = "model",
#'   by = "fv_dist", x = TRUE
#' )
#'
#' library(ggplot2)
#' autoplot(ms, par = "trt")
#' autoplot(ms, par = "trt", type = "lolly", stats = "cover")
#' autoplot(ms, par = "trt", type = "zip")
#' autoplot(ms, par = "trt", type = "est_ba")
autoplot.multisimsum <- function(object, par, type = "forest", stats = "nsim", target = NULL, fitted = TRUE, scales = "fixed", top = TRUE, density.legend = TRUE, zoom = 1, zip_ci_colours = "yellow", ...) {
  ### Manipulate object to make it a simsum-like object
  object[["summ"]] <- object[["summ"]][object[["summ"]][[object[["par"]]]] == par, ]
  object[["true"]] <- object[["true"]][par]
  if (type %in% c("zip", "est", "se", "est_ba", "se_ba", "est_density", "se_density", "est_hex", "se_hex", "est_ridge", "se_ridge")) {
    object[["x"]] <- object[["x"]][object[["x"]][[object[["par"]]]] == par, ]
  }
  #
  if (inherits(object, "summary.multisimsum")) {
    class(object) <- c("summary.simsum", "list")
  } else {
    class(object) <- c("simsum", "list")
  }

  # Call autoplot.simsum on the subset of results for a given parameter:
  plot <- autoplot(object = object, type = type, stats = stats, target = target, fitted = fitted, scales = scales, top = top, density.legend = density.legend, zoom = zoom, zip_ci_colours = zip_ci_colours, ...)

  # Add title with parameter of interest
  plot <- plot +
    ggplot2::labs(title = paste0("Parameter: ", par))

  ### Return plot
  return(plot)
}
