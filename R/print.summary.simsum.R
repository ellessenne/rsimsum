#' @title print.summary.simsum
#' @description Print method for `summary.simsum` objects
#' @param x An object of class `summary.simsum`.
#' @param digits Number of significant digits used for printing. Defaults to 4.
#' @param mcse Should Monte Carlo standard errors be reported? If `mcse = FALSE`, confidence intervals based on Monte Carlo standard errors will be reported instead, see [summary.simsum()]. If a `NULL` value is passed, only point estimates are printed regardless of whether Monte Carlo standard errors were computed or not. Defaults to `TRUE`.
#' @param ... Ignored.
#' @export
#'
#' @examples
#' data("MIsim")
#' x <- simsum(
#'   data = MIsim, estvarname = "b", true = 0.5, se = "se",
#'   methodvar = "method"
#' )
#' xs <- summary(x)
#' xs
#'
#' # Printing less significant digits:
#' print(xs, digits = 2)
#'
#' # Printing confidence intervals:
#' print(xs, mcse = FALSE)
#'
#' # Printing values only:
#' print(xs, mcse = NULL)
print.summary.simsum <- function(x, digits = 4, mcse = TRUE, ...) {
  ### Check arguments
  arg_checks <- checkmate::makeAssertCollection()
  # `digits` must be an integer value greater than or equal to zero
  checkmate::assert_int(x = digits, lower = 0, upper = Inf, add = arg_checks)
  checkmate::assert_logical(x = mcse, len = 1, null.ok = TRUE, add = arg_checks)
  ### Report if there are any errors
  if (!arg_checks$isEmpty()) checkmate::reportAssertions(arg_checks)

  ### Make sure users are not asking for the moon
  if (!x$control$mcse) {
    mcse <- NULL
    message("Monte Carlo Standard Errors were not computed!\nDisplaying point estimates only.")
  }
  if (is.null(mcse)) {
    cat("Values are:\n\tPoint Estimate\n")
  } else if (mcse) {
    cat("Values are:\n\tPoint Estimate (Monte Carlo Standard Error)\n")
  } else {
    cat(paste0("Values are:\n\tPoint Estimate (", sprintf("%.0f%%", 100 * (x$ci_level)), " Confidence Interval based on Monte Carlo Standard Errors)\n"))
  }

  ### Format summary table
  x <- .format(x = x, digits = digits, mcse = mcse)

  ### Make names of the summary table
  names(x$summ)[names(x$summ) == "description"] <- "Performance Measure"
  names(x$summ)[names(x$summ) == "est"] <- "Estimate"

  ### Order data.frame with results
  x$summ <- .order(data = x$summ, by = c("Performance Measure", x$methodvar, x$by))

  ### If length(methodvar) > 1 then process multiple columns into one...
  if (length(x$methodvar) > 1) {
    x$summ <- .compact_method_columns(data = x$summ, methodvar = x$methodvar)$data
    x$methodvar <- ":methodvar"
  }

  ### If methodvar, put them side by side
  if (!is.null(x$methodvar)) {
    x$summ <- .bind_methods(data = x$summ, by = x$by, methodvar = x$methodvar)
  }

  ### Split by summary statistics for printing
  x$summ[["Performance Measure"]] <- droplevels(x$summ[["Performance Measure"]])
  output <- .split_by(data = x$summ, by = "Performance Measure")

  ### Print
  for (i in seq_along(output)) {
    cat(paste0("\n", names(output)[i], ":\n"))
    output[[i]][["Performance Measure"]] <- NULL
    print(output[[i]], row.names = FALSE)
  }
}
