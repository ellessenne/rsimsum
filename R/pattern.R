#' @title Draw pattern plots.
#' @description The S3 method [pattern()] produces a pattern plot to visualise estimates against standard errors for different methods.
#' @param obj An object used to select a method.
#' @param ... Further arguments passed to or from other methods.
#' @details `pattern()` requires `simsum` and `multisimsum` objects top be fit with the `x = TRUE` option.
#' @return A [ggplot2::ggplot()] object that can be combined with additional `geom_*`, `scale_*`, `theme_*`, etc.
#' @export

pattern <- function(obj, ...) {
  UseMethod("pattern", obj)
}
