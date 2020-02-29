#' @title autoplot method for simsum objects
#' @description `autoplot` can produce a series of plot to summarise results of simulation studies. See `vignette("plotting", package = "rsimsum")` for further details.
#' @param object An object of class `simsum`.
#' @param type The type of the plot to be produced. Possible choices are: `forest`, `lolly`, `zip`, `est`, `se`, `est_ba`, `se_ba`, `est_ridge`, `se_ridge`, `est_density`, `se_density`, `est_hex`, `se_hex`, `heat`, `nlp`, with `forest` being the default.
#' @param stats Summary statistic to plot, defaults to `bias`. See [summary.simsum()] for further details on supported summary statistics.
#' @param target Target of summary statistic, e.g. 0 for `bias`. Defaults to `NULL`, in which case target will be inferred.
#' @param fitted Superimpose a fitted regression line, useful when `type` = (`est`, `se`, `est_ba`, `se_ba`, `est_density`, `se_density`, `est_hex`, `se_hex`). Defaults to `TRUE`.
#' @param scales Should scales be fixed (`fixed`, the default), free (`free`), or free in one dimension (`free_x`, `free_y`)?
#' @param top Should the legend for a nested loop plot be on the top side of the plot? Defaults to `TRUE`.
#' @param density.legend Should the legend for density and hexbin plots be included? Defaults to `TRUE`.
#' @param zoom A numeric value between 0 and 1 signalling that a zip plot should _zoom_ on the top x% of the plot (to ease interpretation). Defaults to 1, where the whole zip plot is displayed.
#' @param ... Not used.
#'
#' @return A `ggplot` object.
#' @export
#'
#' @examples
#' data("MIsim", package = "rsimsum")
#' s <- rsimsum::simsum(
#'   data = MIsim, estvarname = "b", true = 0.5, se = "se",
#'   methodvar = "method", x = TRUE
#' )
#'
#' library(ggplot2)
#' autoplot(s)
#' autoplot(s, type = "lolly")
#' autoplot(s, type = "est_hex")
#' autoplot(s, type = "zip", zoom = 0.5)
#'
#' # Nested loop plot:
#' data("nlp", package = "rsimsum")
#' s1 <- rsimsum::simsum(
#'   data = nlp, estvarname = "b", true = 0, se = "se",
#'   methodvar = "model", by = c("baseline", "ss", "esigma")
#' )
#' autoplot(s1, stats = "bias", type = "nlp")
autoplot.simsum <- function(object, type = "forest", stats = "bias", target = NULL, fitted = TRUE, scales = "fixed", top = TRUE, density.legend = TRUE, zoom = 1, ...) {
  ### Check arguments
  arg_checks <- checkmate::makeAssertCollection()
  # 'type' must be a single string value, among those allowed
  checkmate::assert_string(x = type, add = arg_checks)
  checkmate::assert_subset(x = type, choices = c("forest", "lolly", "zip", "est", "se", "est_ba", "se_ba", "est_ridge", "se_ridge", "est_density", "se_density", "est_hex", "se_hex", "heat", "nlp"), empty.ok = FALSE, add = arg_checks)
  # 'stats' must be a single string value, among those allowed
  checkmate::assert_string(x = stats, add = arg_checks)
  checkmate::assert_subset(x = stats, choices = c("nsim", "thetamean", "thetamedian", "se2mean", "se2median", "bias", "empse", "mse", "relprec", "modelse", "relerror", "cover", "becover", "power"), empty.ok = FALSE, add = arg_checks)
  # 'target' must be single numeric value, can be null
  checkmate::assert_number(x = target, null.ok = TRUE, na.ok = FALSE, add = arg_checks)
  # 'zoom' needs to be a numeric value between zero and one
  checkmate::assert_number(x = zoom, lower = 0, upper = 1, null.ok = FALSE, na.ok = FALSE, add = arg_checks)
  # 'fitted', 'top', 'density.legend' must be a single boolean value
  checkmate::assert_logical(x = fitted, len = 1, add = arg_checks)
  checkmate::assert_logical(x = top, len = 1, add = arg_checks)
  checkmate::assert_logical(x = density.legend, len = 1, add = arg_checks)
  # 'scales' must be a single string value, among those allowed
  checkmate::assert_string(x = scales, add = arg_checks)
  checkmate::assert_subset(x = scales, choices = c("fixed", "free", "free_x", "free_y"), empty.ok = FALSE, add = arg_checks)
  # If type = 'zip', 'est', 'se', 'est_ba', 'se_ba', 'est_ridge', 'se_ridge' then object$x must be true
  if (type %in% c("zip", "est", "se", "est_ba", "se_ba", "est_ridge", "se_ridge", "est_density", "se_density", "est_hex", "se_hex")) {
    checkmate::assert_true(x = !is.null(object$x), add = arg_checks)
  }
  # Report
  if (!arg_checks$isEmpty()) checkmate::reportAssertions(arg_checks)

  ### All 'vs' plots not meaningful if there are no 'methodvar' to compare
  if (type %in% c("est", "se", "est_ba", "se_ba", "est_density", "se_density", "est_hex", "se_hex") & is.null(object[["methodvar"]])) stop("This plot is not meaningful when no methods are compared", call. = FALSE)

  ### Nested loop plot not meaningful if there are no 'by' factors
  if (type == "nlp" & is.null(object[["by"]])) stop("Nested loop plot not meaningful when no 'by' factors are defined", call. = FALSE)

  ### Extract data
  df <- get_data(object, stats = stats)

  ### Infer target
  if (is.null(target)) {
    if (stats %in% c("cover", "becover", "power")) {
      target <- 0.95
    } else if (stats %in% c("thetamean", "thetamedian")) {
      target <- object[["true"]]
    } else {
      target <- 0
    }
  }

  ### Add CI if it is a summary object
  ci <- inherits(x = object, what = "summary.simsum")

  ### Call internal function to build plot
  plot <- switch(type,
    "forest" = .forest_plot(data = df, methodvar = object$methodvar, by = object$by, stats = stats, ci = ci, target = target, scales = scales),
    "lolly" = .lolly_plot(data = df, methodvar = object$methodvar, by = object$by, stats = stats, ci = ci, target = target, scales = scales),
    "zip" = .zip_plot(data = object$x, estvarname = object$estvarname, se = object$se, true = object$true, methodvar = object$methodvar, by = object$by, control = object$control, summ = object$summ, zoom = zoom), # zip for coverage
    "est" = .vs_plot(data = object$x, b = object$estvarname, methodvar = object$methodvar, by = object$by, fitted = fitted, scales = scales, ba = FALSE),
    "se" = .vs_plot(data = object$x, b = object$se, methodvar = object$methodvar, by = object$by, fitted = fitted, scales = scales, ba = FALSE),
    "est_ba" = .vs_plot(data = object$x, b = object$estvarname, methodvar = object$methodvar, by = object$by, fitted = fitted, scales = scales, ba = TRUE),
    "se_ba" = .vs_plot(data = object$x, b = object$se, methodvar = object$methodvar, by = object$by, fitted = fitted, scales = scales, ba = TRUE),
    "est_ridge" = .ridge_plot(data = object$x, b = object$estvarname, methodvar = object$methodvar, by = object$by),
    "se_ridge" = .ridge_plot(data = object$x, b = object$se, methodvar = object$methodvar, by = object$by),
    "est_density" = .density_plot(data = object$x, b = object$estvarname, methodvar = object$methodvar, by = object$by, fitted = fitted, scales = scales, hex = FALSE, density.legend = density.legend),
    "se_density" = .density_plot(data = object$x, b = object$se, methodvar = object$methodvar, by = object$by, fitted = fitted, scales = scales, hex = FALSE, density.legend = density.legend),
    "est_hex" = .density_plot(data = object$x, b = object$estvarname, methodvar = object$methodvar, by = object$by, fitted = fitted, scales = scales, hex = TRUE, density.legend = density.legend),
    "se_hex" = .density_plot(data = object$x, b = object$se, methodvar = object$methodvar, by = object$by, fitted = fitted, scales = scales, hex = TRUE, density.legend = density.legend),
    "heat" = .heat_plot(data = df, methodvar = object$methodvar, by = object$by, stats = stats),
    "nlp" = .nlp(data = df, methodvar = object$methodvar, by = object$by, stats = stats, target = target, top = top)
  )

  ### Return plot
  return(plot)
}
