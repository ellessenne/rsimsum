# Summarising simsum objects

The [`summary()`](https://rdrr.io/r/base/summary.html) method for
objects of class `simsum` returns confidence intervals for performance
measures based on Monte Carlo standard errors.

## Usage

``` r
# S3 method for class 'simsum'
summary(object, ci_level = 0.95, df = NULL, stats = NULL, ...)
```

## Arguments

- object:

  An object of class `simsum`.

- ci_level:

  Significance level for confidence intervals based on Monte Carlo
  standard errors. Ignored if a `simsum` object with control parameter
  `mcse = FALSE` is passed.

- df:

  Degrees of freedom of a t distribution that will be used to calculate
  confidence intervals based on Monte Carlo standard errors. If `NULL`
  (the default), quantiles of a Normal distribution will be used
  instead. However, using Z-based or t-based confidence intervals is
  valid only for summary statistics such a bias and coverage. Confidence
  intervals for other quantities may not be appropriate, therefore their
  usage is not recommended.

- stats:

  Summary statistics to include; can be a scalar value or a vector (for
  multiple summary statistics at once). Possible choices are:

  - `nsim`, the number of replications with non-missing point estimates
    and standard error.

  - `thetamean`, average point estimate.

  - `thetamedian`, median point estimate.

  - `se2mean`, average variance.

  - `se2median`, median variance.

  - `bias`, bias in point estimate.

  - `rbias`, relative (to the true value) bias in point estimate.

  - `empse`, empirical standard error.

  - `mse`, mean squared error.

  - `relprec`, percentage gain in precision relative to the reference
    method.

  - `modelse`, model-based standard error.

  - `relerror`, relative percentage error in standard error.

  - `cover`, coverage of a nominal `level`\\

  - `becover`, bias corrected coverage of a nominal `level`\\

  - `power`, power of a (1 - `level`)\\

  Defaults to `NULL`, in which case all possible summary statistics are
  included.

- ...:

  Ignored.

## Value

An object of class `summary.simsum`.

## See also

[`simsum()`](https://ellessenne.github.io/rsimsum/reference/simsum.md),
[`print.summary.simsum()`](https://ellessenne.github.io/rsimsum/reference/print.summary.simsum.md)

## Examples

``` r
data("MIsim")
object <- simsum(
  data = MIsim, estvarname = "b", true = 0.5, se = "se",
  methodvar = "method"
)
#> 'ref' method was not specified, CC set as the reference
xs <- summary(object)
xs
#> Values are:
#>  Point Estimate (Monte Carlo Standard Error)
#> 
#> Non-missing point estimates/standard errors:
#>    CC MI_LOGT MI_T
#>  1000    1000 1000
#> 
#> Average point estimate:
#>      CC MI_LOGT   MI_T
#>  0.5168  0.5009 0.4988
#> 
#> Median point estimate:
#>      CC MI_LOGT   MI_T
#>  0.5070  0.4969 0.4939
#> 
#> Average variance:
#>      CC MI_LOGT   MI_T
#>  0.0216  0.0182 0.0179
#> 
#> Median variance:
#>      CC MI_LOGT   MI_T
#>  0.0211  0.0172 0.0169
#> 
#> Bias in point estimate:
#>               CC         MI_LOGT             MI_T
#>  0.0168 (0.0048) 0.0009 (0.0042) -0.0012 (0.0043)
#> 
#> Relative bias in point estimate:
#>               CC         MI_LOGT             MI_T
#>  0.0335 (0.0096) 0.0018 (0.0083) -0.0024 (0.0085)
#> 
#> Empirical standard error:
#>               CC         MI_LOGT            MI_T
#>  0.1511 (0.0034) 0.1320 (0.0030) 0.1344 (0.0030)
#> 
#> % gain in precision relative to method CC:
#>               CC          MI_LOGT             MI_T
#>  0.0000 (0.0000) 31.0463 (3.9375) 26.3682 (3.8424)
#> 
#> Mean squared error:
#>               CC         MI_LOGT            MI_T
#>  0.0231 (0.0011) 0.0174 (0.0009) 0.0181 (0.0009)
#> 
#> Model-based standard error:
#>               CC         MI_LOGT            MI_T
#>  0.1471 (0.0005) 0.1349 (0.0006) 0.1338 (0.0006)
#> 
#> Relative % error in standard error:
#>                CC         MI_LOGT             MI_T
#>  -2.6594 (2.2055) 2.2233 (2.3323) -0.4412 (2.2695)
#> 
#> Coverage of nominal 95% confidence interval:
#>               CC         MI_LOGT            MI_T
#>  0.9430 (0.0073) 0.9490 (0.0070) 0.9430 (0.0073)
#> 
#> Bias-eliminated coverage of nominal 95% confidence interval:
#>               CC         MI_LOGT            MI_T
#>  0.9400 (0.0075) 0.9490 (0.0070) 0.9430 (0.0073)
#> 
#> Power of 5% level test:
#>               CC         MI_LOGT            MI_T
#>  0.9460 (0.0071) 0.9690 (0.0055) 0.9630 (0.0060)
```
