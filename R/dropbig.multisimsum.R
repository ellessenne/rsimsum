#' dropbig.multisimsum
#'
#' @title Return observations dropped by simsum
#' @description `dropbig.multisimsum` returns observations dropped by simsum while computing summary statistics for a simulation study.
#' @param x An object of class `multisimsum`.
#' @param ... Ignored.
#' @return An object of class `dropbig.multisimsum` if `dropbig` was set to `TRUE` when calling `multisimsum`, `NULL` otherwise. It contains two slots: `big_estvarname` and `big_se`. Both slots consist in the portion of the original data that was identified to have estimated values or standard errors exceeding the limits defined by `max` and `semax`.
#' @seealso [multisimsum()], [print.dropbig.multisimsum()]
#' @export
#'
#' @examples
#' data(frailty)
#' ms <- multisimsum(data = frailty, par = "par", true = c(trt = -0.50,
#'    fv = 0.75), estvarname = "b", se = "se", methodvar = "model",
#'    by = "fv_dist", dropbig = TRUE, max = 6, semax = 3)
#' dropbig(ms)
dropbig.multisimsum <- function(x, ...) {
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
