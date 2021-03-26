#' @keywords internal
.compact_method_columns <- function(data, methodvar) {
  reftable <- do.call(expand.grid, lapply(methodvar, FUN = function(x) unique(data[[x]])))
  names(reftable) <- methodvar
  reftable[[":methodvar"]] <- do.call(paste, c(as.list(reftable), sep = ":"))
  data[[".nr"]] <- seq(nrow(data))
  data <- merge(data, reftable, by = methodvar)
  data <- data[order(data[[".nr"]]), ]
  data[, methodvar] <- NULL
  data[, ".nr"] <- NULL
  return(list(data = data, reftable = reftable))
}
