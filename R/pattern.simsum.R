#' @title pattern method for simsum objects
#' @description [pattern()] method for objects of class `simsum`.
#' @param obj An object of class `simsum`.
#' @param gpars Graphical parameters. Must be a named list, with possible parameters:
#' * `alpha`, alpha value of each point on the scatterplot;
#' * `scales`, scale of `x` and `y` axis of each facet.
#' It is possible to redefine all the graphical parameters or a subset only; if not specified, sensible default values will be utilised. Good practice would be adding a colorblind-safe palette, e.g. using [ggthemes::scale_color_colorblind()].
#' @param ... Ignored.
#' @inherit pattern return details
#' @note `pattern()` automatically factorises the variable representing methods if present and if not already a factor; this allows `ggplot2` to appropriately pick a discrete colour scale rather than a continuous one.
#' @export
#' @examples
#' library(rsimsum)
#' library(ggplot2)
#' data("relhaz", package = "rsimsum")
#' s <- simsum(data = relhaz, estvarname = "theta", true = -0.5, se = "se",
#'   methodvar = "model", by = c("n", "baseline"), x = TRUE)
#' pattern(s)

pattern.simsum <- function(obj, gpars = list(), ...) {
  ### Check arguments
  arg_checks <- checkmate::makeAssertCollection()

  # `gpars` must be a list, with well defined components
  checkmate::assert_list(gpars, add = arg_checks)
  checkmate::assert_subset(names(gpars), choices = c("alpha", "scales"), empty.ok = TRUE, add = arg_checks)

  ### Report if there are any errors
  if (!arg_checks$isEmpty()) {
    checkmate::reportAssertions(arg_checks)
  }

  ### Stop if obj was computed with `x = FALSE`
  if (is.null(obj[["data"]])) stop("obj was computed with 'x = FALSE'. Please re-run simsum setting 'x = TRUE'.")

  ### Graphics control parameters
  gpars.default <- list(alpha = 1 / 3, scales = "free")
  gpars.ok <- unlist(list(
    gpars[names(gpars) %in% names(gpars.default)],
    gpars.default[!(names(gpars.default) %in% names(gpars))]
  ), recursive = FALSE)

  ### Factorise `methodvar` if defined and if it is not already a factor to obtain a proper colour scale
  if (!is.null(obj[["methodvar"]])) {
    if (!("factor" %in% class(obj[["data"]][[obj[["methodvar"]]]]))) {
      obj[["data"]][[obj[["methodvar"]]]] <- stats::relevel(factor(obj[["data"]][[obj[["methodvar"]]]]), ref = obj[["ref"]])
    }
  }

  ### Create plot to return
  gg <- ggplot2::ggplot(obj[["data"]], ggplot2::aes_string(x = obj[["se"]], y = obj[["estvarname"]], color = obj[["methodvar"]])) +
    ggplot2::geom_point(alpha = gpars.ok$alpha)

  ### Add faceting by `by` factors if defined
  if (!is.null(obj[["by"]])) {
    gg <- gg +
      ggplot2::facet_wrap(facets = obj[["by"]], labeller = ggplot2::label_both, scales = gpars.ok$scales)
  }

  ### Return gg object
  return(gg)
}
