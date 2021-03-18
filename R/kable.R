#' @title Create 'kable's
#'
#' @description Create tables in LaTeX, HTML, Markdown, or reStructuredText from objects of class `simsum`, `summary.simsum`, `multisimsum`, `summary.multisimsum`.
#'
#' @param x An object of class `simsum`, `summary.simsum`, `multisimsum`, `summary.multisimsum`;
#' @param stats Summary statistics to include. See [tidy()] for more details;
#' @param digits Maximum number of digits for numeric columns;
#' @param ... Further arguments passed to [knitr::kable()].
#' @seealso [knitr::kable()]
#' @export
kable.simsum <- function(x, stats = NULL, digits = max(3, getOption("digits") - 3), ...) {
  # Get dataset
  out <- tidy(x, stats = stats)
  # Pass dataset to knitr::kable
  knitr::kable(x = out, digits = digits, ...)
}

#' @rdname kable.simsum
#' @export
kable.summary.simsum <- function(x, stats = NULL, digits = max(3, getOption("digits") - 3), ...) {
  kable.simsum(x = x, stats = stats, digits = digits, ...)
}

#' @rdname kable.simsum
#' @export
kable.multisimsum <- function(x, stats = NULL, digits = max(3, getOption("digits") - 3), ...) {
  kable.simsum(x = x, stats = stats, digits = digits, ...)
}

#' @rdname kable.simsum
#' @export
kable.summary.multisimsum <- function(x, stats = NULL, digits = max(3, getOption("digits") - 3), ...) {
  kable.simsum(x = x, stats = stats, digits = digits, ...)
}

#' @rdname kable.simsum
#' @export
kable <- function(x, ...) {
  UseMethod("kable", x)
}
