#' @title Draw forest plots.
#' @description The S3 method [forest()] produces a forest plot to visually summarise the results of a simulation study analysed using [simsum()].
#' @param obj An object used to select a method.
#' @param ... Further arguments passed to or from other methods.
#'
#' @details Coverage, bias corrected coverage, and power will be based on the \eqn{\alpha} defined in the call to `simsum`.
#'
#' @return A [ggplot2::ggplot()] object that can be combined with additional `geom_*`, `scale_*`, `theme_*`, etc.
#' @export

forest <- function(obj, ...) {
  UseMethod("forest", obj)
}
