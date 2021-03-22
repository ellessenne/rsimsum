#' @keywords internal
.compact_method_columns <- function(data, methodvar) {
  reftable <- do.call(expand.grid, lapply(methodvar, FUN = function(x) unique(data[[x]])))
  names(reftable) <- methodvar
  reftable[[":methodvar"]] <- do.call(paste, c(as.list(reftable), sep = ":"))
  data <- merge(data, reftable, by = methodvar, all.x = TRUE, sort = FALSE)
  data[, methodvar] <- NULL
  return(list(data = data, reftable = reftable))
}
