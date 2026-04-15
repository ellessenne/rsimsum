# Summarising multisimsum objects

The [`summary()`](https://rdrr.io/r/base/summary.html) method for
objects of class `multisimsum` returns confidence intervals for
performance measures based on Monte Carlo standard errors.

## Usage

``` r
# S3 method for class 'multisimsum'
summary(object, ci_level = 0.95, df = NULL, stats = NULL, ...)
```

## Arguments

- object:

  An object of class `multisimsum`.

- ci_level:

  Significance level for confidence intervals based on Monte Carlo
  standard errors. Ignored if a `multisimsum` object with control
  parameter `mcse = FALSE` is passed.

- df:

  Degrees of freedom of a t distribution that will be used to calculate
  confidence intervals based on Monte Carlo standard errors. If `NULL`
  (the default), quantiles of a Normal distribution will be used
  instead.

- stats:

  Summary statistics to include; can be a scalar value or a vector (for
  multiple summary statistics at once). Possible choices are:

  - `nsim`, the number of replications with non-missing point estimates
    and standard error.

  - `thetamean`, average point estimate.

  - `thetamedian`, median point estimate.

  - `se2mean`, average standard error.

  - `se2median`, median standard error.

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

  - `power`, power of a (1 - `level`)\\ Defaults to `NULL`, in which
    case all possible summary statistics are included.

- ...:

  Ignored.

## Value

An object of class `summary.multisimsum`.

## See also

[`multisimsum()`](https://ellessenne.github.io/rsimsum/reference/multisimsum.md),
[`print.summary.multisimsum()`](https://ellessenne.github.io/rsimsum/reference/print.summary.multisimsum.md)

## Examples

``` r
data(frailty)
ms <- multisimsum(
  data = frailty, par = "par", true = c(
    trt = -0.50,
    fv = 0.75
  ), estvarname = "b", se = "se", methodvar = "model",
  by = "fv_dist"
)
#> 'ref' method was not specified, Cox, Gamma set as the reference
sms <- summary(ms)
sms
#> Values are:
#>  Point Estimate (Monte Carlo Standard Error)
#> 
#>    
#> Parameter: fv 
#> 
#> Non-missing point estimates/standard errors:
#>     fv_dist Cox, Gamma Cox, Log-Normal RP(P), Gamma RP(P), Log-Normal
#>       Gamma        976            1000          971              1000
#>  Log-Normal        957            1000          997              1000
#> 
#> Average point estimate:
#>     fv_dist Cox, Gamma Cox, Log-Normal RP(P), Gamma RP(P), Log-Normal
#>       Gamma     0.7376          0.9799       0.7321            0.9847
#>  Log-Normal     0.6436          0.7325       0.6434            0.7348
#> 
#> Median point estimate:
#>     fv_dist Cox, Gamma Cox, Log-Normal RP(P), Gamma RP(P), Log-Normal
#>       Gamma     0.7271          0.9566       0.7225            0.9597
#>  Log-Normal     0.6365          0.7182       0.6324            0.7199
#> 
#> Average variance:
#>     fv_dist Cox, Gamma Cox, Log-Normal RP(P), Gamma RP(P), Log-Normal
#>       Gamma     0.0203          0.0600       0.0202            0.0498
#>  Log-Normal     0.0156          0.0230       0.0158            0.0254
#> 
#> Median variance:
#>     fv_dist Cox, Gamma Cox, Log-Normal RP(P), Gamma RP(P), Log-Normal
#>       Gamma     0.0193          0.0483       0.0191            0.0442
#>  Log-Normal     0.0149          0.0206       0.0149            0.0235
#> 
#> Bias in point estimate:
#>     fv_dist       Cox, Gamma  Cox, Log-Normal     RP(P), Gamma
#>       Gamma -0.0124 (0.0045)  0.2299 (0.0076) -0.0179 (0.0044)
#>  Log-Normal -0.1064 (0.0043) -0.0175 (0.0049) -0.1066 (0.0041)
#>  RP(P), Log-Normal
#>    0.2347 (0.0077)
#>   -0.0152 (0.0050)
#> 
#> Relative bias in point estimate:
#>     fv_dist       Cox, Gamma  Cox, Log-Normal     RP(P), Gamma
#>       Gamma -0.0165 (0.0061)  0.3066 (0.0101) -0.0239 (0.0059)
#>  Log-Normal -0.1419 (0.0057) -0.0233 (0.0066) -0.1421 (0.0055)
#>  RP(P), Log-Normal
#>    0.3130 (0.0103)
#>   -0.0203 (0.0066)
#> 
#> Empirical standard error:
#>     fv_dist      Cox, Gamma Cox, Log-Normal    RP(P), Gamma RP(P), Log-Normal
#>       Gamma 0.1421 (0.0032) 0.2406 (0.0054) 0.1387 (0.0031)   0.2438 (0.0055)
#>  Log-Normal 0.1320 (0.0030) 0.1554 (0.0035) 0.1307 (0.0029)   0.1570 (0.0035)
#> 
#> % gain in precision relative to method Cox, Gamma:
#>     fv_dist      Cox, Gamma   Cox, Log-Normal    RP(P), Gamma RP(P), Log-Normal
#>       Gamma 0.0000 (0.0000) -65.1290 (0.6149) 5.0048 (0.0507) -66.0342 (0.5911)
#>  Log-Normal 0.0000 (0.0000) -27.8283 (1.5037) 2.0492 (0.0466) -29.3058 (1.4591)
#> 
#> Mean squared error:
#>     fv_dist      Cox, Gamma Cox, Log-Normal    RP(P), Gamma RP(P), Log-Normal
#>       Gamma 0.0203 (0.0010) 0.1107 (0.0055) 0.0195 (0.0009)   0.1145 (0.0057)
#>  Log-Normal 0.0287 (0.0010) 0.0244 (0.0011) 0.0284 (0.0010)   0.0248 (0.0012)
#> 
#> Model-based standard error:
#>     fv_dist      Cox, Gamma Cox, Log-Normal    RP(P), Gamma RP(P), Log-Normal
#>       Gamma 0.1426 (0.0008) 0.2449 (0.0027) 0.1420 (0.0008)   0.2232 (0.0019)
#>  Log-Normal 0.1249 (0.0008) 0.1517 (0.0013) 0.1258 (0.0007)   0.1594 (0.0011)
#> 
#> Relative % error in standard error:
#>     fv_dist       Cox, Gamma  Cox, Log-Normal     RP(P), Gamma
#>       Gamma  0.3574 (2.3463)  1.7896 (2.5449)  2.3922 (2.3950)
#>  Log-Normal -5.3912 (2.2452) -2.3382 (2.3301) -3.7112 (2.2300)
#>  RP(P), Log-Normal
#>   -8.4531 (2.1890)
#>    1.5422 (2.3713)
#> 
#> Coverage of nominal 95% confidence interval:
#>     fv_dist      Cox, Gamma Cox, Log-Normal    RP(P), Gamma RP(P), Log-Normal
#>       Gamma 0.9201 (0.0087) 0.9220 (0.0085) 0.9300 (0.0082)   0.9030 (0.0094)
#>  Log-Normal 0.7503 (0.0140) 0.9020 (0.0094) 0.7683 (0.0134)   0.9280 (0.0082)
#> 
#> Bias-eliminated coverage of nominal 95% confidence interval:
#>     fv_dist      Cox, Gamma Cox, Log-Normal    RP(P), Gamma RP(P), Log-Normal
#>       Gamma 0.9334 (0.0080) 0.8980 (0.0096) 0.9434 (0.0074)   0.8930 (0.0098)
#>  Log-Normal 0.9164 (0.0089) 0.9130 (0.0089) 0.9308 (0.0080)   0.9360 (0.0077)
#> 
#> Power of 5% level test:
#>     fv_dist      Cox, Gamma Cox, Log-Normal    RP(P), Gamma RP(P), Log-Normal
#>       Gamma 1.0000 (0.0000) 1.0000 (0.0000) 1.0000 (0.0000)   1.0000 (0.0000)
#>  Log-Normal 1.0000 (0.0000) 1.0000 (0.0000) 1.0000 (0.0000)   1.0000 (0.0000)
#> 
#>  -------------------------------------------------------------------------------- 
#>  
#> Parameter: trt 
#> 
#> Non-missing point estimates/standard errors:
#>     fv_dist Cox, Gamma Cox, Log-Normal RP(P), Gamma RP(P), Log-Normal
#>       Gamma       1000            1000          971              1000
#>  Log-Normal       1000            1000          997              1000
#> 
#> Average point estimate:
#>     fv_dist Cox, Gamma Cox, Log-Normal RP(P), Gamma RP(P), Log-Normal
#>       Gamma    -0.5006         -0.5013      -0.5003           -0.5015
#>  Log-Normal    -0.5006         -0.5014      -0.5006           -0.5016
#> 
#> Median point estimate:
#>     fv_dist Cox, Gamma Cox, Log-Normal RP(P), Gamma RP(P), Log-Normal
#>       Gamma    -0.5011         -0.5021      -0.5010           -0.5025
#>  Log-Normal    -0.5014         -0.5021      -0.5014           -0.5022
#> 
#> Average variance:
#>     fv_dist Cox, Gamma Cox, Log-Normal RP(P), Gamma RP(P), Log-Normal
#>       Gamma     0.0026          0.0026       0.0026            0.0026
#>  Log-Normal     0.0023          0.0023       0.0023            0.0023
#> 
#> Median variance:
#>     fv_dist Cox, Gamma Cox, Log-Normal RP(P), Gamma RP(P), Log-Normal
#>       Gamma     0.0026          0.0026       0.0026            0.0026
#>  Log-Normal     0.0022          0.0022       0.0022            0.0022
#> 
#> Bias in point estimate:
#>     fv_dist       Cox, Gamma  Cox, Log-Normal     RP(P), Gamma
#>       Gamma -0.0006 (0.0016) -0.0013 (0.0016) -0.0003 (0.0016)
#>  Log-Normal -0.0006 (0.0015) -0.0014 (0.0015) -0.0006 (0.0015)
#>  RP(P), Log-Normal
#>   -0.0015 (0.0016)
#>   -0.0016 (0.0015)
#> 
#> Relative bias in point estimate:
#>     fv_dist      Cox, Gamma Cox, Log-Normal    RP(P), Gamma RP(P), Log-Normal
#>       Gamma 0.0011 (0.0032) 0.0027 (0.0032) 0.0006 (0.0033)   0.0031 (0.0032)
#>  Log-Normal 0.0012 (0.0030) 0.0028 (0.0030) 0.0013 (0.0030)   0.0032 (0.0030)
#> 
#> Empirical standard error:
#>     fv_dist      Cox, Gamma Cox, Log-Normal    RP(P), Gamma RP(P), Log-Normal
#>       Gamma 0.0508 (0.0011) 0.0509 (0.0011) 0.0506 (0.0011)   0.0509 (0.0011)
#>  Log-Normal 0.0474 (0.0011) 0.0474 (0.0011) 0.0473 (0.0011)   0.0474 (0.0011)
#> 
#> % gain in precision relative to method Cox, Gamma:
#>     fv_dist      Cox, Gamma  Cox, Log-Normal    RP(P), Gamma RP(P), Log-Normal
#>       Gamma 0.0000 (0.0000) -0.4457 (0.1133) 0.4394 (0.0845)  -0.5782 (0.1641)
#>  Log-Normal 0.0000 (0.0000) -0.1417 (0.1367) 0.0918 (0.0853)  -0.2078 (0.1589)
#> 
#> Mean squared error:
#>     fv_dist      Cox, Gamma Cox, Log-Normal    RP(P), Gamma RP(P), Log-Normal
#>       Gamma 0.0026 (0.0001) 0.0026 (0.0001) 0.0026 (0.0001)   0.0026 (0.0001)
#>  Log-Normal 0.0022 (0.0001) 0.0022 (0.0001) 0.0022 (0.0001)   0.0022 (0.0001)
#> 
#> Model-based standard error:
#>     fv_dist      Cox, Gamma Cox, Log-Normal    RP(P), Gamma RP(P), Log-Normal
#>       Gamma 0.0506 (0.0000) 0.0507 (0.0000) 0.0506 (0.0000)   0.0507 (0.0000)
#>  Log-Normal 0.0475 (0.0000) 0.0475 (0.0000) 0.0475 (0.0000)   0.0475 (0.0000)
#> 
#> Relative % error in standard error:
#>     fv_dist       Cox, Gamma  Cox, Log-Normal     RP(P), Gamma
#>       Gamma -0.2017 (2.2346) -0.3890 (2.2304) -0.0544 (2.2710)
#>  Log-Normal  0.2507 (2.2438)  0.1815 (2.2423)  0.3319 (2.2490)
#>  RP(P), Log-Normal
#>   -0.4330 (2.2294)
#>    0.2101 (2.2429)
#> 
#> Coverage of nominal 95% confidence interval:
#>     fv_dist      Cox, Gamma Cox, Log-Normal    RP(P), Gamma RP(P), Log-Normal
#>       Gamma 0.9500 (0.0069) 0.9490 (0.0070) 0.9506 (0.0070)   0.9500 (0.0069)
#>  Log-Normal 0.9410 (0.0075) 0.9420 (0.0074) 0.9428 (0.0074)   0.9430 (0.0073)
#> 
#> Bias-eliminated coverage of nominal 95% confidence interval:
#>     fv_dist      Cox, Gamma Cox, Log-Normal    RP(P), Gamma RP(P), Log-Normal
#>       Gamma 0.9500 (0.0069) 0.9500 (0.0069) 0.9506 (0.0070)   0.9490 (0.0070)
#>  Log-Normal 0.9420 (0.0074) 0.9400 (0.0075) 0.9428 (0.0074)   0.9410 (0.0075)
#> 
#> Power of 5% level test:
#>     fv_dist      Cox, Gamma Cox, Log-Normal    RP(P), Gamma RP(P), Log-Normal
#>       Gamma 1.0000 (0.0000) 1.0000 (0.0000) 1.0000 (0.0000)   1.0000 (0.0000)
#>  Log-Normal 1.0000 (0.0000) 1.0000 (0.0000) 1.0000 (0.0000)   1.0000 (0.0000)
```
