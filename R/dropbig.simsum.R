#' @title Return observations dropped by simsum
#' @description `dropbig.simsum` returns observations dropped by simsum while computing summary statistics for a simulation study.
#' @param x An object of class `simsum`.
#' @param ... Ignored.
#' @return An object of class `dropbig.simsum` if `dropbig` was set to `TRUE` when calling `simsum`, `NULL` otherwise. It contains two slots: `big_estvarname` and `big_se`. Both slots consist in the portion of the original data that was identified to have estimated values or standard errors exceeding the limits defined by `max` and `semax`.
#' @seealso [simsum()], [print.dropbig.simsum()]
#' @export
#'
#' @examples
#' data("MIsim")
#' x <- simsum(
#'   data = MIsim, estvarname = "b", true = 0.5, se = "se",
#'   methodvar = "method", mcse = TRUE,
#'   dropbig = TRUE, max = 3, semax = 1.5
#' )
#' d <- dropbig(x)
#' d
dropbig.simsum <- function(x, ...) {
  obj <- list()
  if (x$dropbig) {
    obj$big_estvarname <- x$big_estvarname
    obj$big_se <- x$big_se
  } else {
    message("`dropbig = FALSE`, no point estimate / standard error was dropped. ")
    return(NULL)
  }
  class(obj) <- c("dropbig.simsum")
  return(obj)
}
