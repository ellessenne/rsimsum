#' @title Identify replications with large point estimates, standard errors
#' @description `dropbig` is useful to identify replications with large point estimates or standard errors. Large values are defined as standardised values above a given threshold, as defined when calling `dropbig`. Regular standardisation using mean and standard deviation is implemented, as well as robust standardisation using median and inter-quartile range. Further to that, the  standardisation process is stratified by data-generating mechanism if `by` factors are defined.
#'
#' @param data A `data.frame` in which variable names are interpreted. It has to be in tidy format, e.g. each variable forms a column and each observation forms a row.
#' @param estvarname The name of the variable containing the point estimates.
#' @param se The name of the variable containing the standard errors of the point estimates.
#' @param methodvar The name of the variable containing the methods to compare. For instance, methods could be the models compared within a simulation study. Can be `NULL`.
#' @param by A vector of variable names to compute performance measures by a list of factors. Factors listed here are the (potentially several) data-generating mechanisms used to simulate data under different scenarios (e.g. sample size, true distribution of a variable, etc.). Can be `NULL`.
#' @param max Specifies the maximum acceptable absolute value of the point estimates, after standardisation. Defaults to 10.
#' @param semax Specifies the maximum acceptable absolute value of the standard error, after standardisation. Defaults to 100.
#' @param robust Specifies whether to use robust standardisation (using median and inter-quartile range) rather than normal standardisation (using mean and standard deviation). Defaults to `TRUE`.
#'
#' @return The same `data.frame` given as input with an additional column named `.dropbig` identifying rows that are classified as large (`.dropbig = TRUE`) according to the specified criterion.
#' @export
#'
#' @examples
#' data("frailty", package = "rsimsum")
#' frailty2 <- subset(frailty, par == "fv")
#' 
#' # Using low values of max, semax for illustration purposes:
#' dropbig(
#'   data = frailty2, estvarname = "b", se = "se",
#'   methodvar = "model", by = "fv_dist", max = 2, semax = 2
#' )
#' 
#' # Using regular standardisation:
#' dropbig(
#'   data = frailty2, estvarname = "b", se = "se",
#'   methodvar = "model", by = "fv_dist", max = 2, semax = 2, robust = FALSE
#' )
dropbig <- function(data, estvarname, se, methodvar = NULL, by = NULL, max = 10, semax = 100, robust = TRUE) {
  ### Check arguments
  arg_checks <- checkmate::makeAssertCollection()
  # 'data' must be a data.frame
  checkmate::assert_data_frame(x = data, add = arg_checks)
  # 'estvarname', 'se', 'methodvar' must be a single string value
  checkmate::assert_string(x = estvarname, add = arg_checks)
  checkmate::assert_string(x = se, add = arg_checks)
  checkmate::assert_string(x = methodvar, null.ok = TRUE, add = arg_checks)
  # 'robust' must be single logical value
  checkmate::assert_logical(x = robust, len = 1, add = arg_checks)
  # 'by' must be a vector of strings; can be NULL
  checkmate::assert_character(x = by, null.ok = TRUE, add = arg_checks)
  # 'estvarname', 'se' must be in 'data'; all elements of 'by' must be in 'data'; 'methodvar' must be in 'data'
  checkmate::assert_subset(x = estvarname, choices = names(data), add = arg_checks)
  checkmate::assert_subset(x = se, choices = names(data), add = arg_checks)
  checkmate::assert_subset(x = by, choices = names(data), add = arg_checks)
  checkmate::assert_subset(x = methodvar, choices = names(data), add = arg_checks)
  # 'max', 'semax' must be a single number
  checkmate::assert_number(x = max, add = arg_checks)
  checkmate::assert_number(x = semax, add = arg_checks)
  # Report
  if (!arg_checks$isEmpty()) checkmate::reportAssertions(arg_checks)

  ### Uses the internal function .dropbig
  return(.dropbig(data = data, estvarname = estvarname, se = se, methodvar = methodvar, by = by, max = max, semax = semax, robust = robust, internal = FALSE))
}
