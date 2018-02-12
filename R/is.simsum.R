#' @title is.simsum
#' @description Reports whether x is a simsum object
#' @param x An object to test.
#' @export
is.simsum <- function(x) {
  inherits(x, "simsum")
}
