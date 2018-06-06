#' @title Analysis of Simulation Studies Including Monte Carlo Error
#'
#' @description Summarise results from simulation studies and compute Monte Carlo
#' standard errors of commonly used summary statistics. This package is modelled
#' on the 'simsum' user-written command in 'Stata' (See White I.R., 2010
#' <http://www.stata-journal.com/article.html?article=st0200>).
#'
#' @name rsimsum
#' @docType package
#' @author Alessandro Gasparini (ag475@@leicester.ac.uk)
#' @import checkmate ggplot2 stats
NULL

# Quiets concerns of R CMD check re: variables used with
# non-standard evaluation within ggplot2's 'aes'
# e.g.: lolly(), zip()
if (getRversion() >= "2.15.1") utils::globalVariables(c("est", "mcse", "lower", "upper", "pr", "colour", "cupper", "clower"))
