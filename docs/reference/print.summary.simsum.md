# print.summary.simsum

Print method for `summary.simsum` objects

## Usage

``` r
# S3 method for class 'summary.simsum'
print(x, digits = 4, mcse = TRUE, ...)
```

## Arguments

- x:

  An object of class `summary.simsum`.

- digits:

  Number of significant digits used for printing. Defaults to 4.

- mcse:

  Should Monte Carlo standard errors be reported? If `mcse = FALSE`,
  confidence intervals based on Monte Carlo standard errors will be
  reported instead, see
  [`summary.simsum()`](https://ellessenne.github.io/rsimsum/reference/summary.simsum.md).
  If a `NULL` value is passed, only point estimates are printed
  regardless of whether Monte Carlo standard errors were computed or
  not. Defaults to `TRUE`.

- ...:

  Ignored.

## Examples

``` r
data("MIsim")
x <- simsum(
  data = MIsim, estvarname = "b", true = 0.5, se = "se",
  methodvar = "method"
)
#> 'ref' method was not specified, CC set as the reference
xs <- summary(x)
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

# Printing less significant digits:
print(xs, digits = 2)
#> Values are:
#>  Point Estimate (Monte Carlo Standard Error)
#> 
#> Non-missing point estimates/standard errors:
#>    CC MI_LOGT MI_T
#>  1000    1000 1000
#> 
#> Average point estimate:
#>    CC MI_LOGT MI_T
#>  0.52    0.50 0.50
#> 
#> Median point estimate:
#>    CC MI_LOGT MI_T
#>  0.51    0.50 0.49
#> 
#> Average variance:
#>    CC MI_LOGT MI_T
#>  0.02    0.02 0.02
#> 
#> Median variance:
#>    CC MI_LOGT MI_T
#>  0.02    0.02 0.02
#> 
#> Bias in point estimate:
#>           CC     MI_LOGT         MI_T
#>  0.02 (0.00) 0.00 (0.00) -0.00 (0.00)
#> 
#> Relative bias in point estimate:
#>           CC     MI_LOGT         MI_T
#>  0.03 (0.01) 0.00 (0.01) -0.00 (0.01)
#> 
#> Empirical standard error:
#>           CC     MI_LOGT        MI_T
#>  0.15 (0.00) 0.13 (0.00) 0.13 (0.00)
#> 
#> % gain in precision relative to method CC:
#>           CC      MI_LOGT         MI_T
#>  0.00 (0.00) 31.05 (3.94) 26.37 (3.84)
#> 
#> Mean squared error:
#>           CC     MI_LOGT        MI_T
#>  0.02 (0.00) 0.02 (0.00) 0.02 (0.00)
#> 
#> Model-based standard error:
#>           CC     MI_LOGT        MI_T
#>  0.15 (0.00) 0.13 (0.00) 0.13 (0.00)
#> 
#> Relative % error in standard error:
#>            CC     MI_LOGT         MI_T
#>  -2.66 (2.21) 2.22 (2.33) -0.44 (2.27)
#> 
#> Coverage of nominal 95% confidence interval:
#>           CC     MI_LOGT        MI_T
#>  0.94 (0.01) 0.95 (0.01) 0.94 (0.01)
#> 
#> Bias-eliminated coverage of nominal 95% confidence interval:
#>           CC     MI_LOGT        MI_T
#>  0.94 (0.01) 0.95 (0.01) 0.94 (0.01)
#> 
#> Power of 5% level test:
#>           CC     MI_LOGT        MI_T
#>  0.95 (0.01) 0.97 (0.01) 0.96 (0.01)

# Printing confidence intervals:
print(xs, mcse = FALSE)
#> Values are:
#>  Point Estimate (95% Confidence Interval based on Monte Carlo Standard Errors)
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
#>                       CC                  MI_LOGT                      MI_T
#>  0.0168 (0.0074, 0.0261) 0.0009 (-0.0073, 0.0091) -0.0012 (-0.0095, 0.0071)
#> 
#> Relative bias in point estimate:
#>                       CC                  MI_LOGT                      MI_T
#>  0.0335 (0.0148, 0.0523) 0.0018 (-0.0145, 0.0182) -0.0024 (-0.0190, 0.0143)
#> 
#> Empirical standard error:
#>                       CC                 MI_LOGT                    MI_T
#>  0.1511 (0.1445, 0.1577) 0.1320 (0.1262, 0.1378) 0.1344 (0.1285, 0.1403)
#> 
#> % gain in precision relative to method CC:
#>                        CC                    MI_LOGT                       MI_T
#>  0.0000 (-0.0000, 0.0000) 31.0463 (23.3290, 38.7636) 26.3682 (18.8372, 33.8991)
#> 
#> Mean squared error:
#>                       CC                 MI_LOGT                    MI_T
#>  0.0231 (0.0209, 0.0253) 0.0174 (0.0157, 0.0191) 0.0181 (0.0163, 0.0198)
#> 
#> Model-based standard error:
#>                       CC                 MI_LOGT                    MI_T
#>  0.1471 (0.1461, 0.1481) 0.1349 (0.1338, 0.1361) 0.1338 (0.1327, 0.1350)
#> 
#> Relative % error in standard error:
#>                         CC                  MI_LOGT                      MI_T
#>  -2.6594 (-6.9820, 1.6633) 2.2233 (-2.3480, 6.7946) -0.4412 (-4.8894, 4.0070)
#> 
#> Coverage of nominal 95% confidence interval:
#>                       CC                 MI_LOGT                    MI_T
#>  0.9430 (0.9286, 0.9574) 0.9490 (0.9354, 0.9626) 0.9430 (0.9286, 0.9574)
#> 
#> Bias-eliminated coverage of nominal 95% confidence interval:
#>                       CC                 MI_LOGT                    MI_T
#>  0.9400 (0.9253, 0.9547) 0.9490 (0.9354, 0.9626) 0.9430 (0.9286, 0.9574)
#> 
#> Power of 5% level test:
#>                       CC                 MI_LOGT                    MI_T
#>  0.9460 (0.9320, 0.9600) 0.9690 (0.9583, 0.9797) 0.9630 (0.9513, 0.9747)

# Printing values only:
print(xs, mcse = NULL)
#> Values are:
#>  Point Estimate
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
#>      CC MI_LOGT    MI_T
#>  0.0168  0.0009 -0.0012
#> 
#> Relative bias in point estimate:
#>      CC MI_LOGT    MI_T
#>  0.0335  0.0018 -0.0024
#> 
#> Empirical standard error:
#>      CC MI_LOGT   MI_T
#>  0.1511  0.1320 0.1344
#> 
#> % gain in precision relative to method CC:
#>      CC MI_LOGT    MI_T
#>  0.0000 31.0463 26.3682
#> 
#> Mean squared error:
#>      CC MI_LOGT   MI_T
#>  0.0231  0.0174 0.0181
#> 
#> Model-based standard error:
#>      CC MI_LOGT   MI_T
#>  0.1471  0.1349 0.1338
#> 
#> Relative % error in standard error:
#>       CC MI_LOGT    MI_T
#>  -2.6594  2.2233 -0.4412
#> 
#> Coverage of nominal 95% confidence interval:
#>      CC MI_LOGT   MI_T
#>  0.9430  0.9490 0.9430
#> 
#> Bias-eliminated coverage of nominal 95% confidence interval:
#>      CC MI_LOGT   MI_T
#>  0.9400  0.9490 0.9430
#> 
#> Power of 5% level test:
#>      CC MI_LOGT   MI_T
#>  0.9460  0.9690 0.9630
```
