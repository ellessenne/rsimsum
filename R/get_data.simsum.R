#' get_data.simsum
#'
#' @title Extract data from a simsum object
#' @param x An object of class `simsum`.
#' @param ... Ignored.
#' @return A `data.frame` containing summary statistics from a simulation study.
#' @seealso [simsum()], [get_data()]
#' @export
#'
#' @examples
#' data(MIsim)
#' x <- simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se",
#'             methodvar = "method", mcse = TRUE)
#' get_data(x)

get_data.simsum <- function(x, ...) {
  x$summ
}
