#' Coerce to a Data Frame
#'
#' @description Functions to coerce an object of class `simsum` to a `data.frame`.
#' @param obj an object of class `simsum`.
#' @param tidy return a tidy dataset or not? Defaults to `FALSE`.
#'
#' @return
#' @export
#'
#' @examples
#'

as.data.frame.simsum <- function(obj, tidy = FALSE) {
	outer = lapply(X = names(obj),
								 FUN = function(x) {
								 	df = data.frame(unlist(obj[[x]]))
								 	if (tidy) {
								 		names(df) = "value"
								 		df$method = x
								 		df$perfms = row.names(df)
								 		row.names(df) = NULL
								 	} else {
								 		names(df) = x
								 	}
								 	return(df)
								 })
	if (tidy) {
		out = do.call(rbind.data.frame, outer)
	} else {
		out = do.call(cbind.data.frame, outer)
		out$perfms = row.names(out)
		row.names(out) = NULL
	}
	return(out)
}

print.simsum <- function() {

}
