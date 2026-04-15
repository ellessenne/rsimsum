# Example of a simulation study on survival modelling

A dataset from a simulation study assessing the impact of misspecifying
the baseline hazard in survival models on regression coefficients. One
thousand datasets were simulated, each containing a binary treatment
variable with a log-hazard ratio of -0.50. Survival data was simulated
for two different sample sizes, 50 and 250 individuals, and under two
different baseline hazard functions, exponential and Weibull.
Consequently, a Cox model (Cox, 1972), a fully parametric exponential
model, and a Royston-Parmar (Royston and Parmar, 2002) model with two
degrees of freedom were fit to each simulated dataset. See
[`vignette("B-relhaz", package = "rsimsum")`](https://ellessenne.github.io/rsimsum/articles/B-relhaz.md)
for more information.

## Usage

``` r
relhaz
```

## Format

A data frame with 1,200 rows and 6 variables:

- `dataset` Simulated dataset number.

- `n` Sample size of the simulate dataset.

- `baseline` Baseline hazard function of the simulated dataset.

- `model` Method used (`Cox`, `Exp`, or `RP(2)`).

- `theta` Point estimate for the log-hazard ratio.

- `se` Standard error of the point estimate.

## References

Cox D.R. 1972. Regression models and life-tables. Journal of the Royal
Statistical Society, Series B (Methodological) 34(2):187-220.
[doi:10.1007/978-1-4612-4380-9_37](https://doi.org/10.1007/978-1-4612-4380-9_37)

Royston, P. and Parmar, M.K. 2002. Flexible parametric
proportional-hazards and proportional-odds models for censored survival
data, with application to prognostic modelling and estimation of
treatment effects. Statistics in Medicine 21(15):2175-2197
[doi:10.1002/sim.1203](https://doi.org/10.1002/sim.1203)

## Examples

``` r
data("relhaz", package = "rsimsum")
```
