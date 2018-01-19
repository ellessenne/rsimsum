#' get_data.summary.simsum
#'
#' @title Extract data from a summary.simsum object
#' @param x An object of class `summary.simsum`.
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

get_data.summary.simsum <- function(x, ...) {
  x$summ
}
