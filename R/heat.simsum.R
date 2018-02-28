#' @title heat method for simsum objects
#' @description [heat()] method for objects of class `simsum`.
#' @param obj An object of class `simsum`.
#' @param sstat Summary statistic to plot. Possible choices are: `nsim`, number of replications without missing estimates / standard errors; `thetamean`, average estimated value; `thetamedian`, median estimated value; `se2mean`, average estimated standard error; `se2median`, median estimated standard error; `bias`, bias in point estimate; `empse`, empirical standard error; `mse`, mean squared error; `relprec`, percentage gain in precision relative to the reference method; `modelse`, model-based standard error; `relerror`, relative percentage error in standard error; `cover`, coverage of nominal \eqn{(1 - \alpha)}\% CI; `bccover`, bias corrected coverage of nominal \eqn{(1 - \alpha)}\% CI; `power`, power of \eqn{\alpha}\% level test.
#' @param y Factor on the `y`-axis of the plot.
#' @param target Target value for the summary statistic of interest. If `NULL` (the default), the target value is inferred (except for `sstat = nsim`).
#' @param text Including estimates and Monte Carlo standard errors in the plot? Defaults to `FALSE`.
#' @param gpars Graphical parameters. Must be a named list, with possible parameters:
#' * `target.colour`, colour representing the target value;
#' * `low.colour`, colour representing the lowest value;
#' * `high.colour`, colour representing the highest value;
#' * `fmt`, format string passed to [base::sprintf()] to format values and Monte Carlo standard errors.
#'
#' It is possible to redefine all the graphical parameters of a subset only; if not specified, sensible default values will be utilised. The default colours are colorblind-friendly.
#' @param ... Ignored.
#' @inherit heat return details
#' @export
#' @examples
#' library(rsimsum)
#' library(ggplot2)
#' data("relhaz", package = "rsimsum")
#' s <- simsum(
#'   data = relhaz, estvarname = "theta", true = -0.5, se = "se",
#'   methodvar = "model", by = c("n", "baseline")
#' )
#' heat(s, sstat = "bias", y = "baseline")
#' heat(s, sstat = "bias", y = "n", text = TRUE)
heat.simsum <- function(obj, sstat, y, target = NULL, text = FALSE, gpars = list(), ...) {
  ### Check arguments
  arg_checks <- checkmate::makeAssertCollection()

  # `sstat` must be a single string value, among a set of possible names
  checkmate::assert_string(sstat, add = arg_checks)
  checkmate::assert_subset(sstat, choices = c("nsim", "thetamean", "thetamedian", "se2mean", "se2median", "bias", "empse", "mse", "relprec", "modelse", "relerror", "cover", "bccover", "power"), add = arg_checks)

  # `y` must be a single value in obj$by
  checkmate::assert_choice(y, choices = obj[["by"]], add = arg_checks)

  # `target` must be a numeric value, can be NULL
  checkmate::assert_number(target, null.ok = TRUE, add = arg_checks)

  # `text` must be single logical value
  checkmate::assert_logical(text, len = 1, add = arg_checks)

  # `gpars` must be a list, with well defined components
  checkmate::assert_list(gpars, add = arg_checks)
  checkmate::assert_subset(names(gpars), choices = c("target.colour", "low.colour", "high.colour", "fmt", "text.size", "text.hjust", "text.vjust"), empty.ok = TRUE, add = arg_checks)

  ### Report if there are any errors
  if (!arg_checks$isEmpty()) {
    checkmate::reportAssertions(arg_checks)
  }

  ### Define new `by`, e.g. everything that is in `by` and not in `y`
  by <- obj[["by"]][!(obj[["by"]] %in% y)]
  # Message about identified `by` factors
  if (length(by) > 0) message("Wrapping factors: ", paste(by, collapse = ","))

  ### Graphics control parameters
  gpars.default <- list(target.colour = "white", low.colour = "#56B4E9", high.colour = "#D55E00", fmt = "%.4f", text.size = 2.5, text.hjust = 0.5, text.vjust = 0.5)
  gpars.ok <- unlist(list(
    gpars[names(gpars) %in% names(gpars.default)],
    gpars.default[!(names(gpars.default) %in% names(gpars))]
  ), recursive = FALSE)

  ### Identify target, min, max values
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
  minest <- min(get_data(obj)[get_data(obj)[["stat"]] == sstat, "est"])
  maxest <- max(get_data(obj)[get_data(obj)[["stat"]] == sstat, "est"])

  ### Factorise `methodvar` if defined and if it is not already a factor to obtain a proper colour scale
  if (!is.null(obj[["methodvar"]])) {
  	if (!("factor" %in% class(obj[["summ"]][[obj[["methodvar"]]]]))) {
  		obj[["summ"]][[obj[["methodvar"]]]] <- stats::relevel(factor(obj[["summ"]][[obj[["methodvar"]]]]), ref = obj[["ref"]])
  	}
  }

  ### Build a ggplot object
  gg <- ggplot2::ggplot(get_data(obj)[get_data(obj)[["stat"]] == sstat, ], ggplot2::aes_string(x = obj[["methodvar"]], y = y, fill = "est")) +
    ggplot2::geom_tile() +
    ggplot2::labs(fill = sstat, caption = sstat)

  # Add filling scale
  if (sstat %in% c("nsim", "se2mean", "se2median", "empse", "mse", "modelse")) {
    gg <- gg +
      ggplot2::scale_fill_gradient(low = gpars.ok$target.colour, high = gpars.ok$high.colour, limits = c(0, max(minest, maxest, target)))
  } else if (sstat %in% c("thetamean", "thetamedian", "bias", "relprec", "relerror")) {
    gg <- gg +
      ggplot2::scale_fill_gradient2(low = gpars.ok$low.colour, mid = gpars.ok$target.colour, high = gpars.ok$high.colour, midpoint = target, limits = c(-1, 1) * max(abs(minest), abs(maxest), abs(target)))
  } else {
    gg <- gg +
      ggplot2::scale_fill_gradientn(colors = c(gpars.ok$low.colour, gpars.ok$target.colour, gpars.ok$high.colour), values = c(0, target, 1), limits = c(0, 1))
  }

  # Add text if required
  if (text) {
    if (obj[["mcse"]] & sstat %in% c("bias", "empse", "mse", "relprec", "modelse", "relerror", "cover", "bccover", "power")) {
      gg <- gg +
        ggplot2::geom_text(ggplot2::aes(label = paste(sprintf(fmt = gpars.ok$fmt, est), "\n(", sprintf(fmt = gpars.ok$fmt, mcse), ")")), size = gpars.ok$text.size, hjust = gpars.ok$text.hjust, vjust = gpars.ok$text.vjust)
    } else {
      gg <- gg +
        ggplot2::geom_text(ggplot2::aes(label = sprintf(fmt = gpars.ok$fmt, est)), size = gpars.ok$text.size, hjust = gpars.ok$text.hjust, vjust = gpars.ok$text.vjust)
    }
  }

  # Faceting by remaining `by` factors
  if (length(by) > 0) {
    gg <- gg +
      ggplot2::facet_wrap(facets = by, labeller = ggplot2::label_both)
  }

  ### Return gg object
  return(gg)
}
