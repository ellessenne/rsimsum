#' @title miss
#' @description Obtain basic information on the proportion of missing data within the results of a simulation study.
#' @param data A `data.frame` in which variable names are interpreted. It has to be in tidy format, e.g. each variable forms a column and each observation forms a row.
#' @param estvarname The name of the variable containing the point estimates.
#' @param se The name of the variable containing the standard errors of the point estimates.
#' @param par The name of the variable containing the different estimands from the simulation study. Can be `NULL`.
#' @param methodvar The name of the variable containing the methods to compare. Can be `NULL`.
#' @param by A vector of variable names to compute performance measures by a list of factors. Can be `NULL`.
#' @return An object of class `miss`.
#' @export
#' @examples
#' library(rsimsum)
#' data("frailty", package = "rsimsum")
#' miss(
#'   data = frailty, estvarname = "b", se = "se", par = "par",
#'   methodvar = "model", by = "fv_dist"
#' )
miss <- function(data, estvarname, se, par = NULL, methodvar = NULL, by = NULL) {
  ### Check arguments
  arg_checks <- checkmate::makeAssertCollection()

  # `data` must be a data.frame
  checkmate::assert_data_frame(data, add = arg_checks)

  # `estvarname`, `se`, `methodvar`, `par`, `fmt` must be a single string value
  checkmate::assert_string(estvarname, add = arg_checks)
  checkmate::assert_string(se, add = arg_checks)
  checkmate::assert_string(par, null.ok = TRUE, add = arg_checks)
  checkmate::assert_string(methodvar, null.ok = TRUE, add = arg_checks)

  # `by` must be a vector of strings; can be NULL
  checkmate::assert_character(by, null.ok = TRUE, add = arg_checks)

  # `estvarname`, `se` must be in `data`; `par` must be in `data`; all elements of `by` must be in data; `methodvar` must be in data
  checkmate::assert_subset(estvarname, choices = names(data), add = arg_checks)
  checkmate::assert_subset(se, choices = names(data), add = arg_checks)
  checkmate::assert_subset(par, choices = names(data), add = arg_checks)
  checkmate::assert_subset(by, choices = names(data), add = arg_checks)
  checkmate::assert_subset(methodvar, choices = names(data), add = arg_checks)

  ### Report if there are any errors
  if (!arg_checks$isEmpty()) {
    checkmate::reportAssertions(arg_checks)
  }

  ### Make dataset with missing indicators
  missdata <- data.frame(
    is.na(data[[estvarname]]),
    is.na(data[[se]])
  )
  names(missdata) <- c(estvarname, se)
  if (!is.null(methodvar)) missdata[[methodvar]] <- data[[methodvar]]
  if (!is.null(by)) missdata <- cbind(missdata, data[by])
  if (!is.null(par)) missdata[[par]] <- data[[par]]

  ### Make summaries of missing values
  ovsumm <- data.frame(
    mean(missdata[[estvarname]]),
    mean(missdata[[se]])
  )
  names(ovsumm) <- paste0("missing_", c(estvarname, se))

  if (!is.null(par)) {
    byp <- split(x = missdata, f = lapply(par, function(x) missdata[[x]]))
    bypsumm <- lapply(seq_along(byp), function(i) {
      out <- data.frame(
        mean(byp[[i]][[estvarname]]),
        mean(byp[[i]][[se]])
      )
      names(out) <- paste0("missing_", c(estvarname, se))
      out[[par]] <- names(byp)[i]
      out
    })
    bypsumm <- do.call(rbind.data.frame, bypsumm)
  }
  if (!is.null(methodvar)) {
    bym <- split(x = missdata, f = lapply(methodvar, function(x) missdata[[x]]))
    bymsumm <- lapply(seq_along(bym), function(i) {
      out <- data.frame(
        mean(bym[[i]][[estvarname]]),
        mean(bym[[i]][[se]])
      )
      names(out) <- paste0("missing_", c(estvarname, se))
      out[[methodvar]] <- names(bym)[i]
      out
    })
    bymsumm <- do.call(rbind.data.frame, bymsumm)
  }
  if (!is.null(by)) {
    byb <- split(x = missdata, f = lapply(by, function(x) missdata[[x]]))
    bybsumm <- lapply(seq_along(byb), function(i) {
      out <- data.frame(
        mean(byb[[i]][[estvarname]]),
        mean(byb[[i]][[se]])
      )
      names(out) <- paste0("missing_", c(estvarname, se))
      byvalues <- unlist(strsplit(names(byb)[i], ".", fixed = TRUE))
      for (w in seq_along(byvalues)) {
        out[[by[w]]] <- byvalues[w]
      }
      out
    })
    bybsumm <- do.call(rbind.data.frame, bybsumm)
  }

  ### Include call and other info into object to return
  obj <- list()
  obj$call <- match.call()
  obj$missdata <- missdata
  obj$estvarname <- estvarname
  obj$se <- se
  obj$par <- par
  obj$methodvar <- methodvar
  obj$by <- by
  obj$ovsumm <- ovsumm
  if (!is.null(par)) {
    obj$bypsumm <- bypsumm
  }
  if (!is.null(methodvar)) {
    obj$bymsumm <- bymsumm
  }
  if (!is.null(by)) {
    obj$bybsumm <- bybsumm
  }

  ### Return object of class missing.simsum
  class(obj) <- c("list", "miss")
  return(obj)
}
