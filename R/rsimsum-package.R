#' @title Analysis of Simulation Studies Including Monte Carlo Error
#'
#' @description Summarise results from simulation studies and compute Monte Carlo
#' standard errors of commonly used summary statistics. This package is modelled
#' on the 'simsum' user-written command in 'Stata' (See White I.R., 2010
#' <http://www.stata-journal.com/article.html?article=st0200>).
#'
#' @name rsimsum
#' @docType package
#' @author Alessandro Gasparini (alessandro.gasparini@@ki.se)
#' @import checkmate ggridges ggplot2 rlang scales stats
NULL

# Quiets concerns of R CMD check re: variable names used internally
if (getRversion() >= "2.15.1") utils::globalVariables(c(".scenario", ".dgm", "X", "Y", "contrast", "cover_lower", "cover_upper", "covering", "est", "lower", "upper", "level"))
