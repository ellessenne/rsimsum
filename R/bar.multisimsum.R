#' @title forest method for multisimsum objects
#' @description [bar()] method for objects of class `multisimsum`.
#' @param obj An object of class `multisimsum`.
#' @param sstat Summary statistic to plot. Possible choices are: `nsim`, number of replications without missing estimates / standard errors; `thetamean`, average estimated value; `thetamedian`, median estimated value; `se2mean`, average estimated standard error; `se2median`, median estimated standard error; `bias`, bias in point estimate; `empse`, empirical standard error; `mse`, mean squared error; `relprec`, percentage gain in precision relative to the reference method; `modelse`, model-based standard error; `relerror`, relative percentage error in standard error; `cover`, coverage of nominal \eqn{(1 - \alpha)}\% CI; `bccover`, bias corrected coverage of nominal \eqn{(1 - \alpha)}\% CI; `power`, power of \eqn{\alpha}\% level test.
#' @param par Estimand to plot. Defaults to `NULL`, in which case the `par` variable from `multisimsum` will be used for faceting.
#' @param by Faceting factors passed to [ggplot2::facet_wrap()]. Defaults to `NULL`, i.e. no faceting.
#' @param target Target value for the summary statistic of interest. If `NULL` (the default), the target value is inferred (except for `sstat = nsim`).
#' @param level Specifies the confidence level for confidence intervals based on Monte Carlo standard errors, produced by default if the `simsum` or `summary.simsum` object passed to `bar` estimated Monte Carlo standard errors (e.g. with `mcse = TRUE`).
#' @param gpars Graphical parameters. Must be a named list, with possible parameters:
#' * `target.shape`, shape of the horizontal line at `target` value;
#' * `target.colour`, colour of the horizontal line at `target` value;
#' * `bar.colour`, colour of each bar;
#' * `bar.fill`, fill of each bar;
#' * `width`, the width of the end of each confidence interval.
#' It is possible to redefine all the graphical parameters of a subset only; if not specified, sensible default values will be utilised.
#' @inherit bar return details
#' @param ... Ignored.
#' @export
#' @examples
#' library(rsimsum)
#' library(ggplot2)
#' data("frailty", package = "rsimsum")
#' ms <- multisimsum(data = frailty, par = "par", true = c(trt = -0.50,
#'    fv = 0.75), estvarname = "b", se = "se", methodvar = "model",
#'    by = "fv_dist")
#' bar(ms, sstat = "bias", par = "trt", by = "fv_dist")

bar.multisimsum <- function(obj, sstat, par = NULL, by = NULL, target = NULL, level = 0.95, gpars = list(), ...) {
  ### Check arguments
  arg_checks <- checkmate::makeAssertCollection()

  # `sstat` must be a single string value, among a set of possible names
  checkmate::assert_string(sstat, add = arg_checks)
  checkmate::assert_subset(sstat, choices = c("nsim", "thetamean", "thetamedian", "se2mean", "se2median", "bias", "empse", "mse", "relprec", "modelse", "relerror", "cover", "bccover", "power"), add = arg_checks)

  # `by` must be in obj$by or NULL
  checkmate::assert_subset(by, choices = obj[["by"]], empty.ok = TRUE, add = arg_checks)

  # `par` must be in unique(obj[[obj$par]])
  checkmate::assert_choice(par, choices = unique(get_data(obj)[[obj[["par"]]]]), null.ok = TRUE, add = arg_checks)

  # `target` must be a numeric value, can be NULL
  checkmate::assert_number(target, null.ok = TRUE, add = arg_checks)

  # `level` must be a numeric value between 0 and 1
  checkmate::assert_number(
    level,
    lower = 0,
    upper = 1,
    add = arg_checks
  )

  # `pars` must be a list, with well defined components
  checkmate::assert_list(gpars, add = arg_checks)
  checkmate::assert_subset(names(gpars), choices = c("target.shape", "target.colour", "bar.colour", "bar.fill", "width"), empty.ok = TRUE, add = arg_checks)

  ### Report if there are any errors
  if (!arg_checks$isEmpty()) {
    checkmate::reportAssertions(arg_checks)
  }

  ### Throw a warning if no `by` specified when calling `bar`, but there is a `by` factor in obj
  if (is.null(by) & !is.null(obj[["by"]])) warning("'obj' contains `by` factors, the resulting plot **may** not be correct.\n\tAlternatively, manually apply faceting.")

  ### Graphics control parameters
  gpars.default <- list(target.shape = 2, target.colour = 2, bar.colour = "gray50", bar.fill = "gray50", width = 1 / 3)
  gpars.ok <- unlist(list(
    gpars[names(gpars) %in% names(gpars.default)],
    gpars.default[!(names(gpars.default) %in% names(gpars))]
  ), recursive = FALSE)

  ### Identify target if target = NULL
  if (is.null(target)) {
    if (sstat == "nsim") stop("'target' is required when sstat is 'nsim'")
    target <- if (sstat %in% c("thetamean", "thetamedian")) {
      obj[["true"]][par]
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
  if (is.null(par)) {
    gg <- ggplot2::ggplot(get_data(summary(obj))[get_data(obj)[["stat"]] == sstat, ], ggplot2::aes_string(x = obj[["methodvar"]], y = "est"))
  } else {
    gg <- ggplot2::ggplot(get_data(summary(obj))[get_data(obj)[["stat"]] == sstat & get_data(summary(obj))[[obj[["par"]]]] == par, ], ggplot2::aes_string(x = obj[["methodvar"]], y = "est", ymin = "lower", ymax = "upper"))
  }
  gg <- gg +
    ggplot2::geom_bar(stat = "identity", colour = gpars.ok$bar.colour, fill = gpars.ok$bar.fill) +
    ggplot2::geom_hline(yintercept = target, linetype = gpars.ok$target.shape, colour = gpars.ok$target.colour) +
    ggplot2::labs(x = sstat)
  # Add 'confidence intervals' if mcse are available
  if (obj[["mcse"]] & sstat %in% c("bias", "empse", "mse", "relprec", "modelse", "relerror", "cover", "bccover", "power")) {
    gg <- gg + ggplot2::geom_errorbar(ggplot2::aes_string(ymin = "lower", ymax = "upper"), width = gpars.ok$width)
  }
  # Facet if `by` is specified
  if (!is.null(by)) {
    if (!is.null(par)) {
      gg <- gg + ggplot2::facet_wrap(facets = by, labeller = ggplot2::label_both)
    } else {
      gg <- gg + ggplot2::facet_grid(stats::reformulate(obj[["par"]], by), labeller = ggplot2::label_both)
    }
  }

  # Add a subtitle with the current parameter estimated if `par` is defined
  if (!is.null(par)) gg <- gg + ggplot2::labs(caption = paste("Parameter:", par))

  ### Return gg object
  return(gg)
}
