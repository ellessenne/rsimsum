#' @import generics ggplot2 knitr rlang scales stats
#' @importFrom checkmate assert_character
#' @importFrom checkmate assert_int
#' @importFrom checkmate assert_list
#' @importFrom checkmate assert_logical
#' @importFrom checkmate assert_number
#' @importFrom checkmate assert_string
#' @importFrom checkmate assert_subset
#' @importFrom checkmate assert_true
#' @importFrom checkmate makeAssertCollection
#' @importFrom checkmate reportAssertions

#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

# Quiets concerns of R CMD check re: variable names used internally
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    ".scenario",
    ".dgm",
    "X",
    "Y",
    "contrast",
    "cover_lower",
    "cover_upper",
    "covering",
    "est",
    "lower",
    "upper",
    "level"
  ))
}
