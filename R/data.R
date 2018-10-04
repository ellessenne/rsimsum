#' @title Example of a simulation study on missing data
#' @description A dataset from a simulation study comparing different ways to handle missing covariates when fitting a Cox model (White and Royston, 2009). One thousand datasets were simulated, each containing normally distributed covariates \eqn{x} and \eqn{z} and time-to-event outcome. Both covariates has 20\% of their values deleted independently of all other variables so the data became missing completely at random (Little and Rubin, 2002). Each simulated dataset was analysed in three ways. A Cox model was fit to the complete cases (`CC`). Then two methods of multiple imputation using chained equations (van Buuren, Boshuizen, and Knook, 1999) were used. The `MI_LOGT` method multiply imputes the missing values of \eqn{x} and \eqn{z} with the outcome included as \eqn{\log (t)} and \eqn{d}, where \eqn{t} is the survival time and \eqn{d} is the event indicator. The \code{MI_T} method is the same except that \eqn{\log (t)} is replaced by \eqn{t} in the imputation model. The results are stored in long format.
#' @format A data frame with 3,000 rows and 4 variables:
#' * `dataset` Simulated dataset number.
#' * `method` Method used (`CC`, `MI_LOGT` or `MI_T`).
#' * `b` Point estimate.
#' * `se` Standard error of the point estimate.
#'
#' @examples
#' data("MIsim", package = "rsimsum")
#' @references White, I.R., and P. Royston. 2009. Imputing missing covariate values for the Cox model. Statistics in Medicine 28(15):1982-1998 \doi{10.1002/sim.3618}
#' @references Little, R.J.A., and D.B. Rubin. 2002. Statistical analysis with missing data. 2nd ed. Hoboken, NJ: Wiley \doi{10.1002/9781119013563}
"MIsim"

#' @title Example of a simulation study on survival modelling
#' @description A dataset from a simulation study assessing the impact of misspecifying the baseline hazard in survival models on regression coefficients. One thousand datasets were simulated, each containing a binary treatment variable with a log-hazard ratio of -0.50. Survival data was simulated for two different sample sizes, 50 and 250 individuals, and under two different baseline hazard functions, exponential and Weibull. Consequently, a Cox model (Cox, 1972), a fully parametric exponential model, and a Royston-Parmar (Royston and Parmar, 2002) model with two degrees of freedom were fit to each simulated dataset.
#' @format A data frame with 12,000 rows and 6 variables:
#' * `dataset` Simulated dataset number.
#' * `n` Sample size of the simulate dataset.
#' * `baseline` Baseline hazard function of the simulated dataset.
#' * `model` Method used (`Cox`, `Exp`, or `RP(2)`).
#' * `theta` Point estimate for the log-hazard ratio.
#' * `se` Standard error of the point estimate.
#'
#' @examples
#' data("relhaz", package = "rsimsum")
#' @references Cox D.R. 1972. Regression models and life-tables. Journal of the Royal Statistical Society, Series B (Methodological) 34(2):187-220. \url{http://www.jstor.org/stable/2985181}
#' @references Royston, P. and Parmar, M.K. 2002. Flexible parametric proportional-hazards and proportional-odds models for censored survival data, with application to prognostic modelling and estimation of treatment effects. Statistics in Medicine 21(15):2175-2197 \doi{10.1002/sim.1203}
"relhaz"

#' @title Example of a simulation study on frailty survival models
#' @description A dataset from a simulation study comparing frailty flexible parametric models fitted using penalised likelihood to semiparametric frailty models. Both models are fitted assuming a Gamma and a log-Normal frailty. One thousand datasets were simulated, each containing a binary treatment variable with a log-hazard ratio of -0.50. Clustered survival data was simulated assuming 50 clusters of 50 individuals each, with a mixture Weibull baseline hazard function and a frailty following either a Gamma or a Log-Normal distribution. The comparison involves estimates of the log-treatment effect, and estimates of heterogeneity (i.e. the estimated frailty variance).
#' @format A data frame with 16,000 rows and 6 variables:
#' * `i` Simulated dataset number.
#' * `b` Point estimate.
#' * `se` Standard error of the point estimate.
#' * `par` The estimand. `trt` is the log-treatment effect, `fv` is the variance of the frailty.
#' * `fv_dist` The true frailty distribution.
#' * `model` Method used (`Cox, Gamma`, `Cox, Log-Normal`, `RP(P), Gamma`, or `RP(P), Log-Normal`).
#'
#' @examples
#' data("frailty", package = "rsimsum")
"frailty"
