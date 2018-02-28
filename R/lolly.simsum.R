#' @title lolly method for simsum objects
#' @description [lolly()] method for objects of class `simsum`.
#' @param obj An object of class `simsum`.
#' @param sstat Summary statistic to plot. Possible choices are: `nsim`, number of replications without missing estimates / standard errors; `thetamean`, average estimated value; `thetamedian`, median estimated value; `se2mean`, average estimated standard error; `se2median`, median estimated standard error; `bias`, bias in point estimate; `empse`, empirical standard error; `mse`, mean squared error; `relprec`, percentage gain in precision relative to the reference method; `modelse`, model-based standard error; `relerror`, relative percentage error in standard error; `cover`, coverage of nominal \eqn{(1 - \alpha)}\% CI; `bccover`, bias corrected coverage of nominal \eqn{(1 - \alpha)}\% CI; `power`, power of \eqn{\alpha}\% level test.
#' @param by Faceting factors passed to [ggplot2::facet_wrap()]. Defaults to `NULL`, i.e. no faceting.
#' @param target Target value for the summary statistic of interest. If `NULL` (the default), the target value is inferred (except for `sstat = nsim`).
#' @param level Specifies the confidence level for confidence intervals based on Monte Carlo standard errors, produced by default if the `simsum` or `summary.simsum` object passed to `lolly` estimated Monte Carlo standard errors (e.g. with `mcse = TRUE`).
#' @param gpars Graphical parameters. Must be a named list, with possible parameters:
#' * `target.shape`, shape of the vertical line at `target` value;
#' * `target.colour`, colour of the vertical line at `target` value;
#' * `segment.shape`, shape of the horizontal segment between `target` and estimated value;
#' * `segment.colour`, colour of the horizontal segment between `target` and estimated value;
#' * `low.end`, shape of the lower end of the confidence interval;
#' * `upp.end`, shape of the upper end of the confidence interval.
#' It is possible to redefine all the graphical parameters of a subset only; if not specified, sensible default values will be utilised.
#' @param ... Ignored.
#' @inherit lolly return details
#' @export
#' @examples
#' library(rsimsum)
#' library(ggplot2)
#' data("relhaz", package = "rsimsum")
#' s <- simsum(data = relhaz, estvarname = "theta", true = -0.5, se = "se",
#'   methodvar = "model", by = c("n", "baseline"))
#' lolly(s, sstat = "bias", by = c("n", "baseline"))
lolly.simsum <- function(obj, sstat, by = NULL, target = NULL, level = 0.95, gpars = list(), ...) {
  ### Check arguments
  arg_checks <- checkmate::makeAssertCollection()

  # `sstat` must be a single string value, among a set of possible names
  checkmate::assert_string(sstat, add = arg_checks)
  checkmate::assert_subset(sstat, choices = c("nsim", "thetamean", "thetamedian", "se2mean", "se2median", "bias", "empse", "mse", "relprec", "modelse", "relerror", "cover", "bccover", "power"), add = arg_checks)

  # `by` must be in obj$by or NULL
  checkmate::assert_subset(by, choices = obj[["by"]], empty.ok = TRUE, add = arg_checks)

  # `target` must be a numeric value, can be NULL
  checkmate::assert_number(target, null.ok = TRUE, add = arg_checks)

  # `level` must be a numeric value between 0 and 1
  checkmate::assert_number(
    level,
    lower = 0,
    upper = 1,
    add = arg_checks
  )

  # `gpars` must be a list, with well defined components
  checkmate::assert_list(gpars, add = arg_checks)
  checkmate::assert_subset(names(gpars), choices = c("target.shape", "target.colour", "segment.shape", "segment.colour", "low.end", "upp.end"), empty.ok = TRUE, add = arg_checks)

  ### Report if there are any errors
  if (!arg_checks$isEmpty()) {
    checkmate::reportAssertions(arg_checks)
  }

  ### Throw a warning if no `by` specified when calling `lolly`, but there is a `by` factor in obj
  if (is.null(by) & !is.null(obj[["by"]])) warning("'obj' contains `by` factors, the resulting plot **may** not be correct.\n\tAlternatively, manually apply faceting.")

  ### Graphics control parameters
  gpars.default <- list(target.shape = 2, target.colour = 2, segment.shape = 3, segment.colour = 1, low.end = 40, upp.end = 41)
  gpars.ok <- unlist(list(
    gpars[names(gpars) %in% names(gpars.default)],
    gpars.default[!(names(gpars.default) %in% names(gpars))]
  ), recursive = FALSE)

  ### Identify target if target = NULL
  if (is.null(target)) {
    if (sstat == "nsim") stop("'target' is required when sstat is 'nsim'")
    target <- if (sstat %in% c("thetamean", "thetamedian")) {
      obj[["true"]]
    } else if (sstat %in% c("se2mean", "se2median", "bias", "empse", "mse", "modelse", "relerror")) {
      0
    } else if (sstat == "relprec") {
      1
    } else if (sstat %in% c("cover", "bccover", "power")) {
      obj[["level"]]
    }
  }

  ### Factorise `methodvar` if defined and if it is not already a factor to obtain a proper colour scale
  if (!is.null(obj[["methodvar"]])) {
  	if (!("factor" %in% class(obj[["summ"]][[obj[["methodvar"]]]]))) {
  		obj[["summ"]][[obj[["methodvar"]]]] <- stats::relevel(factor(obj[["summ"]][[obj[["methodvar"]]]]), ref = obj[["ref"]])
  	}
  }

  ### Build a ggplot object
  gg <- ggplot2::ggplot(get_data(obj)[get_data(obj)[["stat"]] == sstat, ], ggplot2::aes_string(x = "est", y = obj[["methodvar"]], xend = target, yend = obj[["methodvar"]])) +
    ggplot2::geom_vline(xintercept = target, linetype = gpars.ok$target.shape, colour = gpars.ok$target.colour) +
    ggplot2::geom_segment(linetype = gpars.ok$segment.shape, colour = gpars.ok$segment.colour) +
    ggplot2::geom_point() +
    ggplot2::labs(x = sstat)
  # Add 'confidence intervals' if mcse are available
  if (obj[["mcse"]] & sstat %in% c("bias", "empse", "mse", "relprec", "modelse", "relerror", "cover", "bccover", "power")) {
    gg <- gg + ggplot2::geom_point(ggplot2::aes(x = est - stats::qnorm(1 - (1 - level) / 2) * mcse), shape = gpars.ok$low.end) +
      ggplot2::geom_point(ggplot2::aes(x = est + stats::qnorm(1 - (1 - level) / 2) * mcse), shape = gpars.ok$upp.end)
  }
  # Facet if `by` is specified
  if (!is.null(by)) {
    gg <- gg + ggplot2::facet_wrap(facets = by, labeller = ggplot2::label_both)
  }

  ### Return gg object
  return(gg)
}
