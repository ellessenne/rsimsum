#' @title zip method for simsum objects
#' @description [zip()] method for objects of class `simsum`.
#' @param obj An object of class `simsum`.
#' @param gpars Graphical parameters. Must be a named list, with possible parameters:
#' * `ci.alpha`, alpha value of each individual confidence interval;
#' * `true.colour`, colour of the vertical line at `true` value;
#' * `true.shape`, shape of the vertical line at `true` value;
#' * `ci.colour`, colour of the horizontal lines representing confidence intervals for estimated coverage based on Monte Carlo standard errors;
#' * `ci.shape`, shape of the horizontal lines representing confidence intervals for estimated coverage based on Monte Carlo standard errors.
#'
#' It is possible to redefine all the graphical parameters of a subset only; if not specified, sensible default values will be utilised.
#' @param wald.level Confidence level of the Wald test used to compute p-values for sorting each confidence interval. Defaults to `0.95`.
#' @param ... Ignored.
#' @inherit zip return details
#' @export
#' @examples
#' library(rsimsum)
#' library(ggplot2)
#' data("relhaz", package = "rsimsum")
#' s <- simsum(
#'   data = relhaz, estvarname = "theta", true = -0.5, se = "se",
#'   methodvar = "model", by = c("n", "baseline"), x = TRUE
#' )
#' zip(s)
#' data("MIsim", package = "rsimsum")
#' s2 <- simsum(
#'   data = MIsim, estvarname = "b", true = 0.5, se = "se",
#'   methodvar = "method", x = TRUE
#' )
#' zip(s2)
zip.simsum <- function(obj, wald.level = 0.95, gpars = list(), ...) {
  ### Check arguments
  arg_checks <- checkmate::makeAssertCollection()

  # `wald.level` must be a numeric value between 0 and 1
  checkmate::assert_number(
    wald.level,
    lower = 0,
    upper = 1,
    add = arg_checks
  )

  # `gpars` must be a list, with well defined components
  checkmate::assert_list(gpars, add = arg_checks)
  checkmate::assert_subset(names(gpars), choices = c("ci.alpha", "true.colour", "true.shape", "ci.colour", "ci.shape"), empty.ok = TRUE, add = arg_checks)

  ### Report if there are any errors
  if (!arg_checks$isEmpty()) {
    checkmate::reportAssertions(arg_checks)
  }

  ### Stop if obj was computed with `x = FALSE`
  if (is.null(obj[["data"]])) stop("obj was computed with 'x = FALSE'. Please re-run simsum setting 'x = TRUE'.")

  ### Graphics control parameters
  gpars.default <- list(ci.alpha = 1 / 3, true.colour = 7, true.shape = 2, ci.colour = 2, ci.shape = 3)
  gpars.ok <- unlist(list(
    gpars[names(gpars) %in% names(gpars.default)],
    gpars.default[!(names(gpars.default) %in% names(gpars))]
  ), recursive = FALSE)


  ### Perform all calculations required for a zip plot
  ### Different splitting depending on whether methodvar, by are defined
  if (!is.null(obj[["methodvar"]]) & !is.null(obj[["by"]])) {
    bysplit <- split(obj[["data"]], f = lapply(X = obj[["by"]], FUN = function(f) obj[["data"]][[f]]))
    res <- lapply(X = seq_along(bysplit), FUN = function(x) {
      methodsplit <- split(bysplit[[x]], f = lapply(X = obj[["methodvar"]], FUN = function(f) bysplit[[x]][[f]]))
      res <- lapply(X = seq_along(methodsplit), FUN = function(x) {
        d <- data.frame(
          b = methodsplit[[x]][[obj[["estvarname"]]]],
          se = methodsplit[[x]][[obj[["se"]]]]
        )
        d$w <- (d$b - obj[["true"]])^2 / d$se^2
        d$p <- stats::pchisq(d$w, df = 1, lower.tail = FALSE)
        d$order <- rank(-d$p)
        d[[obj[["methodvar"]]]] <- names(methodsplit)[x]
        d
      })
      res <- do.call(rbind.data.frame, res)
      byvalues <- unlist(strsplit(names(bysplit)[x], ".", fixed = TRUE))
      for (w in seq_along(obj[["by"]])) {
        res[[obj[["by"]][w]]] <- byvalues[w]
      }
      res
    })
  } else if (!is.null(obj[["methodvar"]]) & is.null(obj[["by"]])) {
    methodsplit <- split(obj[["data"]], f = lapply(X = obj[["methodvar"]], FUN = function(f) obj[["data"]][[f]]))
    res <- lapply(X = seq_along(methodsplit), FUN = function(x) {
      d <- data.frame(
        b = methodsplit[[x]][[obj[["estvarname"]]]],
        se = methodsplit[[x]][[obj[["se"]]]]
      )
      d$w <- (d$b - obj[["true"]])^2 / d$se^2
      d$p <- stats::pchisq(d$w, df = 1, lower.tail = FALSE)
      d$order <- rank(-d$p)
      d[[obj[["methodvar"]]]] <- names(methodsplit)[x]
      d
    })
  } else if (is.null(obj[["methodvar"]]) & !is.null(obj[["by"]])) {
    bysplit <- split(obj[["data"]], f = lapply(X = obj[["by"]], FUN = function(f) obj[["data"]][[f]]))
    res <- lapply(X = seq_along(bysplit), FUN = function(x) {
      d <- data.frame(
        b = bysplit[[x]][[obj[["estvarname"]]]],
        se = bysplit[[x]][[obj[["se"]]]]
      )
      d$w <- (d$b - obj[["true"]])^2 / d$se^2
      d$p <- stats::pchisq(d$w, df = 1, lower.tail = FALSE)
      d$order <- rank(-d$p)
      byvalues <- unlist(strsplit(names(bysplit)[x], ".", fixed = TRUE))
      for (w in seq_along(obj[["by"]])) {
        d[[obj[["by"]][w]]] <- byvalues[w]
      }
      d
    })
    res
  } else {
    res <- data.frame(
      b = obj[["data"]][[obj[["estvarname"]]]],
      se = obj[["data"]][[obj[["se"]]]]
    )
    res$w <- (res$b - obj[["true"]])^2 / res$se^2
    res$p <- stats::pchisq(res$w, df = 1, lower.tail = FALSE)
    res$order <- rank(-res$p)
  }

  # Turn split lists into a data frame if there is any between `methodvar`, `by`
  if (!is.null(obj[["methodvar"]]) | !is.null(obj[["by"]])) res <- do.call(rbind.data.frame, res)


  # Merge results and compute ranks and covering status
  res$lower <- res$b - stats::qnorm(1 - (1 - obj[["level"]]) / 2) * res$se
  res$upper <- res$b + stats::qnorm(1 - (1 - obj[["level"]]) / 2) * res$se
  res$pr <- res$order / max(res$order)
  res$colour <- factor(ifelse(res$p < 1 - wald.level, "Non-coverer", "Coverer"), levels = c("Coverer", "Non-coverer"))

  # Merge lower and upper confidence interval based on Monte Carlo standard errors for coverage
  y <- get_data(summary(obj))[get_data(obj)["stat"] == "cover", ]
  y[["clower"]] <- y[["lower"]]
  y[["cupper"]] <- y[["upper"]]
  y <- y[, c(obj[["methodvar"]], obj[["by"]], "clower", "cupper")]
  res <- merge(x = res, y = y, by = c(obj[["methodvar"]], obj[["by"]]))

  ### Create plot to return
  gg <- ggplot2::ggplot(res, ggplot2::aes(x = lower, xend = upper, y = pr, yend = pr, colour = colour)) +
    ggplot2::geom_segment(alpha = gpars.ok$ci.alpha) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = cupper), linetype = gpars.ok$ci.shape, colour = gpars.ok$ci.colour) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = clower), linetype = gpars.ok$ci.shape, colour = gpars.ok$ci.colour) +
    ggplot2::geom_vline(xintercept = obj[["true"]], linetype = gpars.ok$true.shape, colour = gpars.ok$true.colour) +
    ggplot2::labs(x = paste0(100 * obj[["level"]], "% confidence intervals"), y = bquote("Centile of ranked p-values for null" ~ theta == .(obj[["true"]])), colour = "") +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::scale_colour_manual(values = c("#56B4E9", "#D55E00"))

  ### Add faceting if `by` or `methovar` are specified
  if (!is.null(obj[["methodvar"]]) & !is.null(obj[["by"]])) {
    gg <- gg +
      ggplot2::facet_grid(stats::reformulate(obj[["by"]], obj[["methodvar"]]), labeller = ggplot2::label_both)
  } else if (is.null(obj[["methodvar"]]) & !is.null(obj[["by"]])) {
    gg <- gg + ggplot2::facet_wrap(facets = obj[["by"]], labeller = ggplot2::label_both)
  } else if (!is.null(obj[["methodvar"]]) & is.null(obj[["by"]])) {
    gg <- gg + ggplot2::facet_wrap(facets = obj[["methodvar"]], labeller = ggplot2::label_both)
  }

  ### Return gg object
  return(gg)
}
