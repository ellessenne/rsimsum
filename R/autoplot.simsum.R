#' @title autoplot method for simsum objects
#' @description `autoplot` can produce a series of plot to summarise results of simulation studies. See `vignette("plotting", package = "rsimsum")` for further details.
#' @param object An object of class `simsum`.
#' @param type The type of the plot to be produced. Possible choices are: `forest`, `lolly`, `zip`, `est`, `se`, `est_ba`, `se_ba`, `est_ridge`, `se_ridge`, `heat`, with `forest` being the default.
#' @param stats Summary statistic to plot, defaults to `bias`. See [summary.simsum()] for further details on supported summary statistics.
#' @param target Target of summary statistic, e.g. 0 for `bias`. Defaults to `NULL`, in which case target will be inferred.
#' @param fitted Superimpose a fitted regression line, useful when `type` = (`est`, `se`, `est_ba`, `se_ba`). Defaults to `TRUE`.
#' @param scales Should scales be fixed (`fixed`, the default), free (`free`), or free in one dimension (`free_x`, `free_y`)?
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
autoplot.simsum <- function(object, type = "forest", stats = "bias", target = NULL, fitted = TRUE, scales = "fixed", ...) {
  ### Check arguments
  arg_checks <- checkmate::makeAssertCollection()
  # 'type' must be a single string value, among those allowed
  checkmate::assert_string(x = type, add = arg_checks)
  checkmate::assert_subset(x = type, choices = c("forest", "lolly", "zip", "est", "se", "est_ba", "se_ba", "est_ridge", "se_ridge", "heat"), empty.ok = FALSE, add = arg_checks)
  # 'stats' must be a single string value, among those allowed
  checkmate::assert_string(x = stats, add = arg_checks)
  checkmate::assert_subset(x = stats, choices = c("nsim", "thetamean", "thetamedian", "se2mean", "se2median", "bias", "empse", "mse", "relprec", "modelse", "relerror", "cover", "becover", "power"), empty.ok = FALSE, add = arg_checks)
  # 'target' must be single numeric value, can be null
  checkmate::assert_number(x = target, null.ok = TRUE, na.ok = FALSE, add = arg_checks)
  # 'fitted' must be a single boolean value
  checkmate::assert_logical(x = fitted, len = 1, add = arg_checks)
  # 'scales' must be a single string value, among those allowed
  checkmate::assert_string(x = scales, add = arg_checks)
  checkmate::assert_subset(x = scales, choices = c("fixed", "free", "free_x", "free_y"), empty.ok = FALSE, add = arg_checks)
  # If type = 'zip', 'est', 'se', 'est_ba', 'se_ba', 'est_ridge', 'se_ridge' then object$x must be true
  if (type %in% c("zip", "est", "se", "est_ba", "se_ba", "est_ridge", "se_ridge")) {
    checkmate::assert_true(x = !is.null(object$x), add = arg_checks)
  }
  # Report
  if (!arg_checks$isEmpty()) checkmate::reportAssertions(arg_checks)

  ### Extract data
  df <- get_data(object)
  df <- df[df$stat == stats, ]

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
  ci <- ifelse("summary.simsum" %in% class(object), TRUE, FALSE)

  ### Call internal function to build plot
  plot <- switch(type,
    "forest" = .forest_plot(data = df, methodvar = object$methodvar, by = object$by, stats = stats, ci = ci, target = target, scales = scales),
    "lolly" = .lolly_plot(data = df, methodvar = object$methodvar, by = object$by, stats = stats, ci = ci, target = target, scales = scales),
    "zip" = .zip_plot(data = object$x, estvarname = object$estvarname, se = object$se, true = object$true, methodvar = object$methodvar, by = object$by, control = object$control, summ = object$summ), # zip for coverage
    "est" = .vs_plot(data = object$x, b = object$estvarname, methodvar = object$methodvar, by = object$by, fitted = fitted, scales = scales, ba = FALSE),
    "se" = .vs_plot(data = object$x, b = object$se, methodvar = object$methodvar, by = object$by, fitted = fitted, scales = scales, ba = FALSE),
    "est_ba" = .vs_plot(data = object$x, b = object$estvarname, methodvar = object$methodvar, by = object$by, fitted = fitted, scales = scales, ba = TRUE),
    "se_ba" = .vs_plot(data = object$x, b = object$se, methodvar = object$methodvar, by = object$by, fitted = fitted, scales = scales, ba = TRUE),
    "est_ridge" = .ridge_plot(data = object$x, b = object$estvarname, methodvar = object$methodvar, by = object$by),
    "se_ridge" = .ridge_plot(data = object$x, b = object$se, methodvar = object$methodvar, by = object$by),
    "heat" = .heat_plot(data = df, methodvar = object$methodvar, by = object$by, stats = stats)
  )

  ### Return plot
  return(plot)
}
