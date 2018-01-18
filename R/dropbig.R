#' dropbig
#'
#' @title Return observations dropped by a function
#' @description `dropbig` returns observations dropped by a function while performing some calculation - for any reason.
#' @param x An object used to select a method.
#' @param ... Further arguments passed to or from other methods.
#'
#' @export

dropbig <- function(x, ...) {
  UseMethod("dropbig", x)
}
