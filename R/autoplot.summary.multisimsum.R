#' @title autoplot method for summary.multisimsum objects
#'
#' @param object An object of class `summary.multisimsum`.
#' @inheritParams autoplot.multisimsum
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
#' sms <- summary(ms)
#'
#' library(ggplot2)
#' autoplot(sms, par = "trt")
#' autoplot(sms, par = "trt", type = "lolly", stats = "cover")
#' autoplot(sms, par = "trt", type = "zip")
#' autoplot(sms, par = "trt", type = "est_ba")
autoplot.summary.multisimsum <- function(object, par, type = "forest", stats = "bias", target = NULL, fitted = TRUE, scales = "fixed", ...) {
  autoplot.multisimsum(object = object, par = par, type = type, stats = stats, target = target, fitted = fitted, scales = scales)
}
