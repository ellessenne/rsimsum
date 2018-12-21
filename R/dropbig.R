#' @title Detect potential outliers
#' @description `dropbig` returns observations dropped by `simsum` or `multisimsum` before calculating summary statistics if `dropbig = TRUE` was set.
#' The algorithm checks whether standardised values of `estvarname` of `se` are larger than `max` and `semax`, respectively.
#' Robust standardisation using median and inter-quartile range is available and utilised by default.
#' @param data The dataset;
#' @param estvarname The variable with the estimated value, as a string;
#' @param se The variable with the estimates standard error, as a string;
#' @param methodvar If defined, the standardisation will be stratified within levels of `methodvar`;
#' @param by If defined, the standardisation will be stratified within levels of `by`;
#' @param max Standardised values of `estvarname` above `max` will be detected as potential outliers. Defaults to 10;
#' @param semax Standardised values of `se` above `semax` will be detected as potential outliers. Defaults to 100;
#' @param robust Whether to use robust standardisation using median and inter-quartile range or regular standardisation using mean and standard deviation. Defaults to `TRUE`.
#' @return A `data.frame` of all potential outliers detected in `data`.
#' @export
#' @examples
#' data("relhaz", package = "rsimsum")
#' dropbig(data = relhaz, estvarname = "theta", se = "se")
#' dropbig(data = relhaz, estvarname = "theta", se = "se", methodvar = "model", by = c("n", "baseline"))
#' dropbig(data = relhaz, estvarname = "theta", se = "se", methodvar = "model", by = c("n", "baseline"), max = 2)
dropbig <- function(data, estvarname, se, methodvar = NULL, by = NULL, max = 10, semax = 100, robust = TRUE) {
  ### Check arguments
  arg_checks <- checkmate::makeAssertCollection()

  # 'data' must be a data.frame
  checkmate::assert_data_frame(data, add = arg_checks)

  # 'estvarname', 'se', 'methodvar' must be a single string value
  checkmate::assert_string(estvarname, add = arg_checks)
  checkmate::assert_string(se, add = arg_checks)
  checkmate::assert_string(methodvar, null.ok = TRUE, add = arg_checks)

  # 'by' must be a vector of strings; can be NULL
  checkmate::assert_character(by, null.ok = TRUE, add = arg_checks)

  # 'estvarname', 'se' must be in 'data'; all elements of 'by' must be in 'data'; 'methodvar' must be in 'data'
  checkmate::assert_subset(estvarname, choices = names(data), add = arg_checks)
  checkmate::assert_subset(se, choices = names(data), add = arg_checks)
  checkmate::assert_subset(by, choices = names(data), add = arg_checks)
  checkmate::assert_subset(methodvar, choices = names(data), add = arg_checks)

  # 'max', 'semax' must be a single numeric value
  checkmate::assert_number(max, add = arg_checks)
  checkmate::assert_number(semax, add = arg_checks)

  # 'robust' must be single logical value
  checkmate::assert_logical(robust, len = 1, add = arg_checks)

  ### Report if there are any errors
  if (!arg_checks$isEmpty()) {
    checkmate::reportAssertions(arg_checks)
  }

  ### Return subset of dataframe with potential outliers
  data[.dropbig(data = data, estvarname = estvarname, se = se, methodvar = methodvar, by = by, max = max, semax = semax, robust = robust, internal = FALSE), , drop = FALSE]
}
