# Example of a simulation study on frailty survival models

A dataset from a simulation study comparing frailty flexible parametric
models fitted using penalised likelihood to semiparametric frailty
models. Both models are fitted assuming a Gamma and a log-Normal
frailty. One thousand datasets were simulated, each containing a binary
treatment variable with a log-hazard ratio of -0.50. Clustered survival
data was simulated assuming 50 clusters of 50 individuals each, with a
mixture Weibull baseline hazard function and a frailty following either
a Gamma or a Log-Normal distribution. The comparison involves estimates
of the log-treatment effect, and estimates of heterogeneity (i.e. the
estimated frailty variance).

## Usage

``` r
frailty

frailty2
```

## Format

A data frame with 16,000 rows and 6 variables:

- `i` Simulated dataset number.

- `b` Point estimate.

- `se` Standard error of the point estimate.

- `par` The estimand. `trt` is the log-treatment effect, `fv` is the
  variance of the frailty.

- `fv_dist` The true frailty distribution.

- `model` Method used (`Cox, Gamma`, `Cox, Log-Normal`, `RP(P), Gamma`,
  or `RP(P), Log-Normal`).

An object of class `data.frame` with 16000 rows and 7 columns.

## Note

`frailty2` is a version of the same dataset with the `model` column
split into two columns, `m_baseline` and `m_frailty`.

## Examples

``` r
data("frailty", package = "rsimsum")
data("frailty2", package = "rsimsum")
```
