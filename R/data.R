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
#' @description A dataset from a simulation study assessing the impact of misspecifying the baseline hazard in survival models on regression coefficients. One thousand datasets were simulated, each containing a binary treatment variable with a log-hazard ratio of -0.50. Survival data was simulated for two different sample sizes, 50 and 250 individuals, and under two different baseline hazard functions, exponential and Weibull. Consequently, a Cox model (Cox, 1972), a fully parametric exponential model, and a Royston-Parmar (Royston and Parmar, 2002) model with two degrees of freedom were fit to each simulated dataset. See `vignette("relhaz", package = "rsimsum")` for more information.
#' @format A data frame with 1,200 rows and 6 variables:
#' * `dataset` Simulated dataset number.
#' * `n` Sample size of the simulate dataset.
#' * `baseline` Baseline hazard function of the simulated dataset.
#' * `model` Method used (`Cox`, `Exp`, or `RP(2)`).
#' * `theta` Point estimate for the log-hazard ratio.
#' * `se` Standard error of the point estimate.
#'
#' @examples
#' data("relhaz", package = "rsimsum")
#' @references Cox D.R. 1972. Regression models and life-tables. Journal of the Royal Statistical Society, Series B (Methodological) 34(2):187-220. \doi{10.1007/978-1-4612-4380-9_37}
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

#' @title Example of a simulation study on survival modelling
#' @description A dataset from a simulation study with 150 data-generating mechanisms, useful to illustrate nested loop plots. This simulation study aims to compare the Cox model and flexible parametric models in a variety of scenarios: different baseline hazard functions, sample size, and varying amount of heterogeneity unaccounted for in the model (simulated as white noise with a given variance). A Cox model and a Royston-Parmar model with 5 degrees of freedom are fit to each replication.
#' @format A data frame with 30,000 rows and 10 variables:
#' * `dgm` Data-generating mechanism, 1 to 150.
#' * `i` Simulated dataset number.
#' * `model` Method used, with 1 the Cox model and 2 the RP(5) model.
#' * `b` Point estimate for the log-hazard ratio.
#' * `se` Standard error of the point estimate.
#' * `baseline` Baseline hazard function of the simulated dataset.
#' * `ss` Sample size of the simulated dataset.
#' * `esigma` Standard deviation of the white noise.
#' * `pars` (Ancillary) Parameters of the baseline hazard function.
#'
#' @note Further details on this simulation study can be found in the R script used to generate this dataset, available on GitHub: [https://github.com/ellessenne/rsimsum/blob/master/data-raw/nlp-data.R](https://github.com/ellessenne/rsimsum/blob/master/data-raw/nlp-data.R)
#' @examples
#' data("nlp", package = "rsimsum")
#' @references Cox D.R. 1972. Regression models and life-tables. Journal of the Royal Statistical Society, Series B (Methodological) 34(2):187-220. \doi{10.1007/978-1-4612-4380-9_37}
#' @references Royston, P. and Parmar, M.K. 2002. Flexible parametric proportional-hazards and proportional-odds models for censored survival data, with application to prognostic modelling and estimation of treatment effects. Statistics in Medicine 21(15):2175-2197 \doi{10.1002/sim.1203}
#' @references Rücker, G. and Schwarzer, G. 2014. Presenting simulation results in a nested loop plot. BMC Medical Research Methodology 14:129 \doi{10.1186/1471-2288-14-129}
"nlp"

#' @title Example of a simulation study on the t-test
#' @description A dataset from a simulation study with 4 data-generating mechanisms, useful to illustrate custom input of confidence intervals to calculate coverage probability.
#' This simulation study aims to compare the t-test assuming pooled or unpooled variance in violation (or not) of the t-test assumptions: normality of data, and equality (or not) or variance between groups.
#' The true value of the difference between groups is -1.
#' @format A data frame with 4,000 rows and 8 variables:
#' * `diff` The difference in mean between groups estimated by the t-test;
#' * `se` Standard error of the estimated difference;
#' * `lower`, `upper` Confidence interval for the difference in mean as reported by the t-test;
#' * `df` The number of degrees of freedom assumed by the t-test;
#' * `repno` Identifies each replication, between 1 and 500;
#' * `dgm` Identifies each data-generating mechanism: 1 corresponds to normal data with equal variance between the groups, 2 is normal data with unequal variance, 3 and 4 are skewed data (simulated from a Gamma distribution) with equal and unequal variance between groups, respectively;
#' * `method` Analysis method: 1 represents the t-test with pooled variance, while 2 represents the t-test with unpooled variance.
#'
#' @note Further details on this simulation study can be found in the R script used to generate this dataset, available on GitHub: [https://github.com/ellessenne/rsimsum/blob/master/data-raw/tt-data.R](https://github.com/ellessenne/rsimsum/blob/master/data-raw/tt-data.R)
#' @examples
#' data("tt", package = "rsimsum")
"tt"
