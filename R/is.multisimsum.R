#' @title is.multisimsum
#' @description Reports whether x is a multisimsum object
#' @param x An object to test.
#' @export
is.multisimsum <- function(x) {
  inherits(x, "multisimsum")
}
