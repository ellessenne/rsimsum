#' @title zip method for multisimsum objects
#' @description [zip()] method for objects of class `multisimsum`.
#' @param obj An object of class `multisimsum`.
#' @param par Estimand to plot.
#' @param gpars Graphical parameters. Must be a named list, with possible parameters:
#' * `ci.alpha`, alpha value of each individual confidence interval;
#' * `true.colour`, colour of the vertical line at `true` value;
#' * `true.shape`, shape of the vertical line at `true` value;
#' * `ci.colour`, colour of the horizontal lines representing confidence intervals for estimated coverage based on Monte Carlo standard errors;
#' * `ci.shape`, shape of the horizontal lines representing confidence intervals for estimated coverage based on Monte Carlo standard errors.
#'
#' It is possible to redefine all the graphical parameters of a subset only; if not specified, sensible default values will be utilised.
#' @param wald.level Confidence level of the Wald test used to compute p-values for sorting each confidence interval. Defaults to `0.95`.
#' @param ... Ignored.
#' @inherit zip return details
#' @export
#' @examples
#' library(rsimsum)
#' library(ggplot2)
#' data("frailty", package = "rsimsum")
#' ms <- multisimsum(
#'   data = frailty, par = "par", true = c(
#'     trt = -0.50,
#'     fv = 0.75
#'   ), estvarname = "b", se = "se", methodvar = "model",
#'   by = "fv_dist", x = TRUE
#' )
#' zip(ms, par = "trt")
zip.multisimsum <- function(obj, par, wald.level = 0.95, gpars = list(), ...) {
  ### Check arguments
  arg_checks <- checkmate::makeAssertCollection()

  # `par` must be in unique(obj[[obj$par]])
  checkmate::assert_choice(par, choices = unique(get_data(obj)[[obj[["par"]]]]), add = arg_checks)

  # `wald.level` must be a numeric value between 0 and 1
  checkmate::assert_number(
    wald.level,
    lower = 0,
    upper = 1,
    add = arg_checks
  )

  # `gpars` must be a list, with well defined components
  checkmate::assert_list(gpars, add = arg_checks)
  checkmate::assert_subset(names(gpars), choices = c("ci.alpha", "true.colour", "true.shape", "ci.colour", "ci.shape"), empty.ok = TRUE, add = arg_checks)

  ### Report if there are any errors
  if (!arg_checks$isEmpty()) {
    checkmate::reportAssertions(arg_checks)
  }

  ### Stop if obj was computed with `x = FALSE`
  if (is.null(obj$data)) stop("obj was computed with 'x = FALSE'. Please re-run simsum setting 'x = TRUE'.")

  ### Graphics control parameters
  gpars.default <- list(ci.alpha = 1 / 3, true.colour = 7, true.shape = 2, ci.colour = 2, ci.shape = 3)
  gpars.ok <- unlist(list(
    gpars[names(gpars) %in% names(gpars.default)],
    gpars.default[!(names(gpars.default) %in% names(gpars))]
  ), recursive = FALSE)

  ### Hack obj to make it resemble a simsum object
  hobj <- obj
  hobj[["true"]] <- obj[["true"]][par]
  hobj[["summ"]] <- obj[["summ"]][obj[["summ"]][["par"]] == par, ]
  hobj[["summ"]][, "par"] <- NULL
  hobj[["data"]] <- obj[["data"]][obj[["data"]][[obj[["par"]]]] == par, ]
  hobj[["data"]][, obj[["par"]]] <- NULL
  class(hobj) <- c("list", "simsum")

  ### Make a zip plot via zip.simsum
  gg <- zip.simsum(hobj, wald.level = wald.level, gpars = gpars)

  # Add a subtitle with the current parameter estimated
  gg <- gg +
    ggplot2::labs(caption = paste("Parameter:", par))

  ### Return gg
  return(gg)
}
