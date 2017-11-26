#' MIsim
#'
#' @title Example of a simulation study
#' @description A dataset from a simulation study comparing different ways to handle missing covariates when fitting a Cox model (White and Royston, 2009). One thousand datasets were simulated, each containing normally distributed covariates \eqn{x} and \eqn{z} and time-to-event outcome. Both covariates has 20\% of their values deleted independently of all other variables so the data became missing completely at random (Little and Rubin, 2002). Each simulated dataset was analysed in three ways. A Cox model was fit to the complete cases (`CC`). Then two methods of multiple imputation using chained equations (van Buuren, Boshuizen, and Knook, 1999) were used. The `MI_LOGT` method multiply imputes the missing values of \eqn{x} and \eqn{z} with the outcome included as \eqn{\log (t)} and \eqn{d}, where \eqn{t} is the survival time and \eqn{d} is the event indicator. The \code{MI_T} method is the same except that \eqn{\log (t)} is replaced by \eqn{t} in the imputation model. The results are stored in long format.
#' @format A data frame with 3,000 rows and 4 variables:
#' * `dataset` Simulated dataset number.
#' * `method` Method used (`CC`, `MI_LOGT` or `MI_T`).
#' * `b` Point estimate.
#' * `se` Standard error of the point estimate.
#'
#' @examples
#' data("MIsim")
#'
#' @references White, I.R., and P. Royston. 2009. Imputing missing covariate values for the Cox model. Statistics in Medicine 28: 1982-1998 [https://doi.org/10.1002/sim.3618](https://doi.org/10.1002/sim.3618)
#' @references Little, R.J.A., and D.B. Rubin. 2002. Statistical analysis with missing data. 2nd ed. Hoboken, NJ: Wiley [https://doi.org/10.1002/9781119013563](https://doi.org/10.1002/9781119013563)
#' @references van Buuren, S., H.C. Boshuizen, and D.L. Knook. 1999. Multiple imputation of missing blood pressure covariates in survival analysis. Statistics in Medicine 18: 681-694 [https://doi.org/10.1002/(SICI)1097-0258(19990330)18:6<681::AID-SIM71>3.0.CO;2-R]([https://doi.org/10.1002/(SICI)1097-0258(19990330)18:6<681::AID-SIM71>3.0.CO;2-R)

"MIsim"
