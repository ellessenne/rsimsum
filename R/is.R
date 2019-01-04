#' @title is.simsum
#' @description Reports whether x is a simsum object
#' @param x An object to test.
#' @export
is.simsum <- function(x) {
  inherits(x, "simsum")
}

#' @title is.summary.simsum
#' @description Reports whether x is a summary.simsum object
#' @param x An object to test.
#' @export
is.summary.simsum <- function(x) {
  inherits(x, "summary.simsum")
}

#' @title is.multisimsum
#' @description Reports whether x is a multisimsum object
#' @param x An object to test.
#' @export
is.multisimsum <- function(x) {
  inherits(x, "multisimsum")
}

#' @title is.summary.multisimsum
#' @description Reports whether x is a summary.multisimsum object
#' @param x An object to test.
#' @export
is.summary.multisimsum <- function(x) {
  inherits(x, "summary.multisimsum")
}
