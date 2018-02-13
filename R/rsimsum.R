#' @title `rsimsum` package
#' @description Summarise results from simulation studies and compute Monte Carlo standard errors of commonly used summary statistics. This package is modelled on the 'simsum' user-written command in 'Stata' (See White I.R., 2010 <http://www.stata-journal.com/article.html?article=st0200>). See the README on \href{https://github.com/ellessenne/rsimsum#readme}{GitHub}.
#'
#' @name rsimsum
NULL

# Quiets concerns of R CMD check re: variables used with
# non-standard evaluation within ggplot2's 'aes'
# e.g.: lolly(), zip()
if (getRversion() >= "2.15.1") utils::globalVariables(c("est", "mcse", "lower", "upper", "pr", "colour", "cupper", "clower"))
