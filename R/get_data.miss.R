#' @title get_data.miss
#' @description Extract data from a miss object
#' @param x An object of class `miss`.
#' @param ... Ignored.
#' @return A `data.frame` with estimates and standard errors substituted for a missingness boolean value.
#' @seealso [miss()], [get_data()]
#' @export
#' @examples
#' data("frailty", package = "rsimsum")
#' m <- miss(data = frailty, estvarname = "b", se = "se", par = "par",
#'             methodvar = "model", by = "fv_dist")
#' get_data(m)
get_data.miss <- function(x, ...) {
  ### Return data
  x$missdata
}
