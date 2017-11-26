#' get_data
#'
#' @title Extract data slot from objects
#' @description `get_data` returns a data set contained in an S3 object.
#' @param x An object used to select a method.
#' @param ... Further arguments passed to or from other methods.
#' @export

get_data <- function(x, ...) {
	UseMethod("get_data", x)
}
