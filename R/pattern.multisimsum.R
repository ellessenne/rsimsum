#' @title pattern method for multisimsum objects
#' @description [pattern()] method for objects of class `multisimsum`.
#' @param obj An object of class `multisimsum`.
#' @param par Estimand to plot.
#' @param gpars Graphical parameters. Must be a named list, with possible parameters:
#' * `alpha`, alpha value of each point on the scatterplot;
#' * `scales`, scale of `x` and `y` axis of each facet.
#' It is possible to redefine all the graphical parameters or a subset only; if not specified, sensible default values will be utilised. Good practice would be adding a colorblind-safe palette, e.g. using [ggthemes::scale_color_colorblind()].
#' @param ... Ignored.
#' @inherit pattern return details
#' @details If `par = NULL` (the default), all estimands are plotted and included as faceting variables.
#' @export
#' @examples
#' library(rsimsum)
#' library(ggplot2)
#' data("frailty", package = "rsimsum")
#' ms <- multisimsum(data = frailty, par = "par", true = c(trt = -0.50, fv = 0.75),
#'                     estvarname = "b", se = "se", methodvar = "model",
#'                     by = "fv_dist", x = TRUE)
#' pattern(ms)
#' pattern(ms, par = "trt")
pattern.multisimsum <- function(obj, par = NULL, gpars = list(), ...) {
  ### Check arguments
  arg_checks <- checkmate::makeAssertCollection()

  # `gpars` must be a list, with well defined components
  checkmate::assert_list(gpars, add = arg_checks)
  checkmate::assert_subset(names(gpars), choices = c("alpha", "scales"), empty.ok = TRUE, add = arg_checks)

  # `par` must be in unique(obj[[obj$par]])
  checkmate::assert_choice(par, choices = unique(get_data(obj)[[obj[["par"]]]]), null.ok = TRUE, add = arg_checks)

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

  ### Hack obj to make it resemble a simsum object if `par` is specified, and call pattern.simsum
  if (!is.null(par)) {
    hobj <- obj
    hobj[["true"]] <- obj[["true"]][par]
    hobj[["summ"]] <- obj[["summ"]][obj[["summ"]][["par"]] == par, ]
    hobj[["summ"]][, "par"] <- NULL
    hobj[["data"]] <- obj[["data"]][obj[["data"]][[obj[["par"]]]] == par, ]
    hobj[["data"]][, obj[["par"]]] <- NULL
    class(hobj) <- c("list", "simsum")
    gg <- pattern(hobj, gpars = gpars) +
      ggplot2::labs(caption = par)
  }

  ### Create plot to return if `par` not defined, with faceting
  if (is.null(par)) {
    gg <- ggplot2::ggplot(obj[["data"]], ggplot2::aes_string(x = obj[["se"]], y = obj[["estvarname"]], color = obj[["methodvar"]])) +
      ggplot2::geom_point(alpha = gpars.ok$alpha)

    ### Add faceting: cols using `by` factors (if defined), rows by `par` (if not defined)
    if (!is.null(obj[["by"]])) {
      gg <- gg +
        ggplot2::facet_wrap(facets = c(obj[["by"]], obj[["par"]]), labeller = ggplot2::label_both, scales = gpars.ok$scales)
    } else {
      gg <- gg +
        ggplot2::facet_wrap(facets = obj[["par"]], labeller = ggplot2::label_both, scales = gpars.ok$scales)
    }
  }
  ### Return gg object
  return(gg)
}
