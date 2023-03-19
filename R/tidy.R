#' @title Turn an object into a tidy dataset
#' @description Extract a tidy dataset with results from an object of class `simsum`, `summary.simsum`, `multisimsum`, or `summary.multisimsum`.
#' @param x An object of class `simsum`.
#' @param stats Summary statistics to include; can be a scalar value or a vector.
#' Possible choices are:
#' * `nsim`, the number of replications with non-missing point estimates and standard error.
#' * `thetamean`, average point estimate.
#' * `thetamedian`, median point estimate.
#' * `se2mean`, average standard error.
#' * `se2median`, median standard error.
#' * `bias`, bias in point estimate.
#' * `rbias`, relative (to the true value) bias in point estimate.
#' * `empse`, empirical standard error.
#' * `mse`, mean squared error.
#' * `relprec`, percentage gain in precision relative to the reference method.
#' * `modelse`, model-based standard error.
#' * `relerror`, relative percentage error in standard error.
#' * `cover`, coverage of a nominal `level`\% confidence interval.
#' * `becover`, bias-eliminated coverage of a nominal `level`\% confidence interval.
#' * `power`, power of a (1 - `level`)\% level test.
#' Defaults to `NULL`, in which case all summary statistics are returned.
#' @param ... Ignored.
#' @return A `data.frame` containing summary statistics from a simulation study.
#' @export
#' @rdname tidy
#'
#' @examples
#' data(MIsim)
#' x <- simsum(
#'   data = MIsim, estvarname = "b", true = 0.5, se = "se",
#'   methodvar = "method"
#' )
#' tidy(x)
#'
#' # Extracting only bias and coverage:
#' tidy(x, stats = c("bias", "cover"))
#'
#' xs <- summary(x)
#' tidy(xs)
tidy.simsum <- function(x, stats = NULL, ...) {
  ### Check arguments
  arg_checks <- checkmate::makeAssertCollection()
  # '.x' must be an object of class 'simsum', 'summary.simsum', 'multisimsum', 'summary.multisimsum' (any)
  checkmate::assert_true(x = inherits(x = x, what = c("simsum", "summary.simsum", "multisimsum", "summary.multisimsum")), add = arg_checks)
  # '.stats' must be one of the possible choices
  checkmate::assert_character(x = stats, null.ok = TRUE, add = arg_checks)
  checkmate::assert_subset(x = stats, choices = c("nsim", "thetamean", "thetamedian", "se2mean", "se2median", "bias", "rbias", "empse", "mse", "relprec", "modelse", "relerror", "cover", "becover", "power"), add = arg_checks)
  ### Report if there are any errors
  if (!arg_checks$isEmpty()) checkmate::reportAssertions(arg_checks)
  ### Select only summary statistics on interest
  if (!is.null(stats)) {
    x$summ <- x$summ[x$summ$stat %in% stats, ]
  }
  ### Remove row.names
  row.names(x$summ) <- NULL
  ### Return data
  return(x$summ)
}

#' @export
#' @rdname tidy
tidy.summary.simsum <- function(x, stats = NULL, ...) tidy.simsum(x, stats, ...)

#' @export
#' @rdname tidy
tidy.multisimsum <- function(x, stats = NULL, ...) tidy.simsum(x, stats, ...)

#' @export
#' @rdname tidy
tidy.summary.multisimsum <- function(x, stats = NULL, ...) tidy.simsum(x, stats, ...)
