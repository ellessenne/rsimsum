#' @title Summarising multisimsum objects
#' @description The `summary()` method for objects of class `multisimsum` returns confidence intervals for performance measures based on Monte Carlo standard errors.
#' @param object An object of class `multisimsum`.
#' @param ci_level Significance level for confidence intervals based on Monte Carlo standard errors. Ignored if a `multisimsum` object with control parameter `mcse = FALSE` is passed.
#' @param df Degrees of freedom of a t distribution that will be used to calculate confidence intervals based on Monte Carlo standard errors. If `NULL` (the default), quantiles of a Normal distribution will be used instead.
#' @param stats Summary statistics to include; can be a scalar value or a vector (for multiple summary statistics at once). Possible choices are:
#' * `nsim`, the number of replications with non-missing point estimates and standard error.
#' * `thetamean`, average point estimate.
#' * `thetamedian`, median point estimate.
#' * `se2mean`, average standard error.
#' * `se2median`, median standard error.
#' * `bias`, bias in point estimate.
#' * `empse`, empirical standard error.
#' * `mse`, mean squared error.
#' * `relprec`, percentage gain in precision relative to the reference method.
#' * `modelse`, model-based standard error.
#' * `relerror`, relative percentage error in standard error.
#' * `cover`, coverage of a nominal `level`\% confidence interval.
#' * `becover`, bias corrected coverage of a nominal `level`\% confidence interval.
#' * `power`, power of a (1 - `level`)\% level test.
#' Defaults to `NULL`, in which case all possible summary statistics are included.
#' @param ... Ignored.
#' @return An object of class `summary.multisimsum`.
#' @seealso [multisimsum()], [print.summary.multisimsum()]
#' @export
#'
#' @examples
#' data(frailty)
#' ms <- multisimsum(
#'   data = frailty, par = "par", true = c(
#'     trt = -0.50,
#'     fv = 0.75
#'   ), estvarname = "b", se = "se", methodvar = "model",
#'   by = "fv_dist"
#' )
#' sms <- summary(ms)
#' sms
summary.multisimsum <- function(object, ci_level = 0.95, df = NULL, stats = NULL, ...) {
  ### Check arguments
  arg_checks <- checkmate::makeAssertCollection()
  # 'level' must be a numeric value
  checkmate::assert_number(ci_level, lower = 0, upper = 1, add = arg_checks)
  # 'stats' must be one of the allowed values, can be NULL
  checkmate::assert_character(x = stats, pattern = "^nsim$|^thetamean$|^thetamedian$|^$|^se2mean$|^se2median$|^bias$|^empse$|^mse$|^relprec$|^modelse$|^relerror$|^cover$|^becover$|^power$", null.ok = TRUE)
  ### Report if there are any errors
  if (!arg_checks$isEmpty()) checkmate::reportAssertions(arg_checks)

  ### Compute confidence intervals if Monte Carlo standard errors are available
  if (object$control$mcse) {
    if (is.null(df)) {
      crit <- stats::qnorm(1 - (1 - ci_level) / 2)
    } else {
      crit <- stats::qt(1 - (1 - ci_level) / 2, df = df)
    }
    object$summ$lower <- object$summ$est - crit * object$summ$mcse
    object$summ$upper <- object$summ$est + crit * object$summ$mcse
  }

  ### Add ci_level slot
  if (object$control$mcse) object$ci_level <- ci_level

  ## If no methodvar is specified, it is not possible to compute relprec
  if (is.null(object$methodvar)) {
    object$summ <- object$summ[object$summ$stat != "relprec", ]
  }

  ## Select only stats that are requested
  if (!is.null(stats)) {
    object$summ <- object$summ[object$summ$stat %in% stats, ]
  }

  ## Remove row.names
  row.names(object$summ) <- NULL

  ### Return object of class summary.simsum
  class(object) <- c("list", "summary.multisimsum")
  return(object)
}
