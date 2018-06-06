#' @title get_data.summary.simsum
#' @description Extract data from a summary.simsum object
#' @param x An object of class `summary.simsum`.
#' @param sstat Summary statistics to include; can be a scalar value or a vector. Possible choices are:
#' * `all`, all the summary statistics are returned This is the default option.
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
#' * `bccover`, bias corrected coverage of a nominal `level`\% confidence interval.
#' * `power`, power of a (1 - `level`)\% level test.#'
#' @param description Append a column to the returned `data.frame` with a description of each summary statistic? Defaults to `FALSE`.
#' @param ... Ignored.
#' @return A `data.frame` containing summary statistics from a simulation study.
#' @seealso [summary.simsum()], [get_data()]
#' @export
#'
#' @examples
#' data(MIsim)
#' x <- simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se",
#'             methodvar = "method", mcse = TRUE)
#' xs <- summary(x)
#' get_data(xs)
#'
#' # Exporting only bias and coverage:
#' get_data(xs, sstat = c("bias", "cover"))
#'
#' # Append a column with a description of each summary statistic exported:
#' get_data(xs, sstat = c("bias", "cover"), description = TRUE)

get_data.summary.simsum <- function(x, sstat = "all", description = FALSE, ...) {
  ### Check arguments
  arg_checks <- checkmate::makeAssertCollection()

  # `sstat` must be one of the possible choices
  checkmate::assert_subset(sstat, choices = c("all", "nsim", "thetamean", "thetamedian", "se2mean", "se2median", "bias", "empse", "mse", "relprec", "modelse", "relerror", "cover", "bccover", "power"), add = arg_checks)

  # `description` must be a boolean value
  checkmate::assert_logical(description, add = arg_checks)

  ### Report if there are any errors
  if (!arg_checks$isEmpty()) checkmate::reportAssertions(arg_checks)

  ### Select only summary statistics on interest
  if (!("all" %in% sstat)) {
    x$summ <- x$summ[x$summ$stat %in% sstat, ]
  }

  ### Add description column if requested
  if (description) {
    x$summ <- merge_description(x$summ, ref = x$ref, level = x$level)
  }

  ### Return data
  x$summ
}
