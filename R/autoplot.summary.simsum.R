#' @title autoplot method for summary.simsum objects
#'
#' @param object An object of class `summary.simsum`.
#' @inheritParams autoplot.simsum
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
#' ss <- summary(s)
#'
#' library(ggplot2)
#' autoplot(ss)
#' autoplot(ss, type = "lolly")
autoplot.summary.simsum <- function(object, type = "forest", stats = "bias", target = NULL, fitted = TRUE, scales = "fixed", ...) {
  autoplot.simsum(object = object, type = type, stats = stats, target = target, fitted = fitted, scales = scales)
}
