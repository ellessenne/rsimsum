#' @import checkmate generics ggridges ggplot2 knitr lifecycle rlang scales stats
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

# Quiets concerns of R CMD check re: variable names used internally
if (getRversion() >= "2.15.1") utils::globalVariables(c(".scenario", ".dgm", "X", "Y", "contrast", "cover_lower", "cover_upper", "covering", "est", "lower", "upper", "level"))
