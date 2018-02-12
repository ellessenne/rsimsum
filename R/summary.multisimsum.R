#' @title Summarising multisimsum objects
#' @description The `summary()` method for objects of class `multisimsum` returns confidence intervals for performance measures based on Monte Carlo standard errors.
#' @param object An object of class `multisimsum`.
#' @param ci_level Significance level for confidence intervals based on Monte Carlo standard errors. Ignored if a `multisimsum` object is obtained with `mcse = FALSE`.
#' @param ... Ignored.
#' @return An object of class `summary.multisimsum`.
#' @seealso [multisimsum()], [print.summary.multisimsum()]
#' @note Confidence intervals are constructed using quantiles from a normal distribution with the specified `ci_level` confidence level, and are therefore symmetric by definition.
#' @export
#'
#' @examples
#' data(frailty)
#' ms <- multisimsum(data = frailty, par = "par", true = c(trt = -0.50,
#'    fv = 0.75), estvarname = "b", se = "se", methodvar = "model",
#'    by = "fv_dist")
#' sms <- summary(ms)
#' sms
summary.multisimsum <- function(object, ci_level = 0.95, ...) {
  ### Check arguments
  arg_checks <- checkmate::makeAssertCollection()

  # `level` must be a numeric value
  checkmate::assert_number(ci_level, lower = 0, upper = 1, add = arg_checks)

  ### Report if there are any errors
  if (!arg_checks$isEmpty()) checkmate::reportAssertions(arg_checks)

  ### Compute confidence intervals if Monte Carlo standard errors are available
  if (object$mcse) {
    object$summ$lower <- object$summ$est - stats::qnorm(1 - (1 - ci_level) / 2) * object$summ$mcse
    object$summ$upper <- object$summ$est + stats::qnorm(1 - (1 - ci_level) / 2) * object$summ$mcse
  }

  ### Add ci_level slot
  if (object$mcse) object$ci_level <- ci_level

  ### Return object of class summary.simsum
  class(object) <- c("list", "summary.multisimsum")
  return(object)
}
