#' @title heat method for simsum objects
#' @description [heat()] method for objects of class `simsum`.
#' @param obj An object of class `simsum`.
#' @param par Estimand to plot.
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
#' library(rsimsum)
#' library(ggplot2)
#' data("frailty", package = "rsimsum")
#' ms <- multisimsum(data = frailty, par = "par", true = c(trt = -0.50,
#'    fv = 0.75), estvarname = "b", se = "se", methodvar = "model",
#'    by = "fv_dist")
#' heat(ms, sstat = "bias", par = "trt", y = "fv_dist", x = TRUE)


heat.multisimsum <- function(obj, par, sstat, y, target = NULL, text = FALSE, gpars = list(), ...) {
  ### Check arguments
  arg_checks <- checkmate::makeAssertCollection()

  # `par` must be in unique(obj[[obj$par]])
  checkmate::assert_choice(par, choices = unique(get_data(obj)[[obj[["par"]]]]), add = arg_checks)

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

  ### Hack obj to make it resemble a simsum object
  hobj <- obj
  hobj[["true"]] <- obj[["true"]][par]
  hobj[["summ"]] <- obj[["summ"]][obj[["summ"]][["par"]] == par, ]
  hobj[["summ"]][, "par"] <- NULL
  hobj[["data"]] <- obj[["data"]][obj[["data"]][[obj[["par"]]]] == par, ]
  hobj[["data"]][, obj[["par"]]] <- NULL
  class(hobj) <- c("list", "simsum")

  ### Make a heat plot via heat.simsum
  gg <- heat.simsum(obj = hobj, sstat = sstat, y = y, target = target, text = text, gpars = gpars)

  # Add a subtitle with the current parameter estimated
  gg <- gg +
    ggplot2::labs(caption = paste("Parameter:", par))

  ### Return gg
  return(gg)
}
