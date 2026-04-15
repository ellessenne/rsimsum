# print.summary.multisimsum

Print method for `summary.multisimsum` objects

## Usage

``` r
# S3 method for class 'summary.multisimsum'
print(x, digits = 4, mcse = TRUE, ...)
```

## Arguments

- x:

  An object of class `summary.multisimsum`.

- digits:

  Number of significant digits used for printing. Defaults to 4.

- mcse:

  Should Monte Carlo standard errors be reported? If `mcse = FALSE`,
  confidence intervals based on Monte Carlo standard errors will be
  reported instead, see
  [`summary.multisimsum()`](https://ellessenne.github.io/rsimsum/reference/summary.multisimsum.md).
  If a `NULL` value is passed, only point estimates are printed
  regardless of whether Monte Carlo standard errors were computed or
  not. Defaults to `TRUE`.

- ...:

  Ignored.

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
sms <- summary(ms, stats = c("bias", "cover", "mse"))
sms
#> Values are:
#>  Point Estimate (Monte Carlo Standard Error)
#> 
#>    
#> Parameter: fv 
#> 
#> Bias in point estimate:
#>     fv_dist       Cox, Gamma  Cox, Log-Normal     RP(P), Gamma
#>       Gamma -0.0124 (0.0045)  0.2299 (0.0076) -0.0179 (0.0044)
#>  Log-Normal -0.1064 (0.0043) -0.0175 (0.0049) -0.1066 (0.0041)
#>  RP(P), Log-Normal
#>    0.2347 (0.0077)
#>   -0.0152 (0.0050)
#> 
#> Mean squared error:
#>     fv_dist      Cox, Gamma Cox, Log-Normal    RP(P), Gamma RP(P), Log-Normal
#>       Gamma 0.0203 (0.0010) 0.1107 (0.0055) 0.0195 (0.0009)   0.1145 (0.0057)
#>  Log-Normal 0.0287 (0.0010) 0.0244 (0.0011) 0.0284 (0.0010)   0.0248 (0.0012)
#> 
#> Coverage of nominal 95% confidence interval:
#>     fv_dist      Cox, Gamma Cox, Log-Normal    RP(P), Gamma RP(P), Log-Normal
#>       Gamma 0.9201 (0.0087) 0.9220 (0.0085) 0.9300 (0.0082)   0.9030 (0.0094)
#>  Log-Normal 0.7503 (0.0140) 0.9020 (0.0094) 0.7683 (0.0134)   0.9280 (0.0082)
#> 
#>  -------------------------------------------------------------------------------- 
#>  
#> Parameter: trt 
#> 
#> Bias in point estimate:
#>     fv_dist       Cox, Gamma  Cox, Log-Normal     RP(P), Gamma
#>       Gamma -0.0006 (0.0016) -0.0013 (0.0016) -0.0003 (0.0016)
#>  Log-Normal -0.0006 (0.0015) -0.0014 (0.0015) -0.0006 (0.0015)
#>  RP(P), Log-Normal
#>   -0.0015 (0.0016)
#>   -0.0016 (0.0015)
#> 
#> Mean squared error:
#>     fv_dist      Cox, Gamma Cox, Log-Normal    RP(P), Gamma RP(P), Log-Normal
#>       Gamma 0.0026 (0.0001) 0.0026 (0.0001) 0.0026 (0.0001)   0.0026 (0.0001)
#>  Log-Normal 0.0022 (0.0001) 0.0022 (0.0001) 0.0022 (0.0001)   0.0022 (0.0001)
#> 
#> Coverage of nominal 95% confidence interval:
#>     fv_dist      Cox, Gamma Cox, Log-Normal    RP(P), Gamma RP(P), Log-Normal
#>       Gamma 0.9500 (0.0069) 0.9490 (0.0070) 0.9506 (0.0070)   0.9500 (0.0069)
#>  Log-Normal 0.9410 (0.0075) 0.9420 (0.0074) 0.9428 (0.0074)   0.9430 (0.0073)

# Printing less significant digits:
print(sms, digits = 3)
#> Values are:
#>  Point Estimate (Monte Carlo Standard Error)
#> 
#>    
#> Parameter: fv 
#> 
#> Bias in point estimate:
#>     fv_dist     Cox, Gamma Cox, Log-Normal   RP(P), Gamma RP(P), Log-Normal
#>       Gamma -0.012 (0.005)   0.230 (0.008) -0.018 (0.004)     0.235 (0.008)
#>  Log-Normal -0.106 (0.004)  -0.017 (0.005) -0.107 (0.004)    -0.015 (0.005)
#> 
#> Mean squared error:
#>     fv_dist    Cox, Gamma Cox, Log-Normal  RP(P), Gamma RP(P), Log-Normal
#>       Gamma 0.020 (0.001)   0.111 (0.005) 0.020 (0.001)     0.114 (0.006)
#>  Log-Normal 0.029 (0.001)   0.024 (0.001) 0.028 (0.001)     0.025 (0.001)
#> 
#> Coverage of nominal 95% confidence interval:
#>     fv_dist    Cox, Gamma Cox, Log-Normal  RP(P), Gamma RP(P), Log-Normal
#>       Gamma 0.920 (0.009)   0.922 (0.008) 0.930 (0.008)     0.903 (0.009)
#>  Log-Normal 0.750 (0.014)   0.902 (0.009) 0.768 (0.013)     0.928 (0.008)
#> 
#>  -------------------------------------------------------------------------------- 
#>  
#> Parameter: trt 
#> 
#> Bias in point estimate:
#>     fv_dist     Cox, Gamma Cox, Log-Normal   RP(P), Gamma RP(P), Log-Normal
#>       Gamma -0.001 (0.002)  -0.001 (0.002) -0.000 (0.002)    -0.002 (0.002)
#>  Log-Normal -0.001 (0.001)  -0.001 (0.001) -0.001 (0.001)    -0.002 (0.001)
#> 
#> Mean squared error:
#>     fv_dist    Cox, Gamma Cox, Log-Normal  RP(P), Gamma RP(P), Log-Normal
#>       Gamma 0.003 (0.000)   0.003 (0.000) 0.003 (0.000)     0.003 (0.000)
#>  Log-Normal 0.002 (0.000)   0.002 (0.000) 0.002 (0.000)     0.002 (0.000)
#> 
#> Coverage of nominal 95% confidence interval:
#>     fv_dist    Cox, Gamma Cox, Log-Normal  RP(P), Gamma RP(P), Log-Normal
#>       Gamma 0.950 (0.007)   0.949 (0.007) 0.951 (0.007)     0.950 (0.007)
#>  Log-Normal 0.941 (0.007)   0.942 (0.007) 0.943 (0.007)     0.943 (0.007)

# Printing confidence intervals:
print(sms, digits = 3, mcse = FALSE)
#> Values are:
#>  Point Estimate (95% Confidence Interval based on Monte Carlo Standard Errors)
#> 
#>    
#> Parameter: fv 
#> 
#> Bias in point estimate:
#>     fv_dist              Cox, Gamma         Cox, Log-Normal
#>       Gamma -0.012 (-0.021, -0.003)    0.230 (0.215, 0.245)
#>  Log-Normal -0.106 (-0.115, -0.098) -0.017 (-0.027, -0.008)
#>             RP(P), Gamma       RP(P), Log-Normal
#>  -0.018 (-0.027, -0.009)    0.235 (0.220, 0.250)
#>  -0.107 (-0.115, -0.098) -0.015 (-0.025, -0.006)
#> 
#> Mean squared error:
#>     fv_dist           Cox, Gamma      Cox, Log-Normal         RP(P), Gamma
#>       Gamma 0.020 (0.018, 0.022) 0.111 (0.100, 0.121) 0.020 (0.018, 0.021)
#>  Log-Normal 0.029 (0.027, 0.031) 0.024 (0.022, 0.027) 0.028 (0.026, 0.030)
#>     RP(P), Log-Normal
#>  0.114 (0.103, 0.126)
#>  0.025 (0.023, 0.027)
#> 
#> Coverage of nominal 95% confidence interval:
#>     fv_dist           Cox, Gamma      Cox, Log-Normal         RP(P), Gamma
#>       Gamma 0.920 (0.903, 0.937) 0.922 (0.905, 0.939) 0.930 (0.914, 0.946)
#>  Log-Normal 0.750 (0.723, 0.778) 0.902 (0.884, 0.920) 0.768 (0.742, 0.794)
#>     RP(P), Log-Normal
#>  0.903 (0.885, 0.921)
#>  0.928 (0.912, 0.944)
#> 
#>  -------------------------------------------------------------------------------- 
#>  
#> Parameter: trt 
#> 
#> Bias in point estimate:
#>     fv_dist             Cox, Gamma        Cox, Log-Normal
#>       Gamma -0.001 (-0.004, 0.003) -0.001 (-0.004, 0.002)
#>  Log-Normal -0.001 (-0.004, 0.002) -0.001 (-0.004, 0.002)
#>            RP(P), Gamma      RP(P), Log-Normal
#>  -0.000 (-0.003, 0.003) -0.002 (-0.005, 0.002)
#>  -0.001 (-0.004, 0.002) -0.002 (-0.005, 0.001)
#> 
#> Mean squared error:
#>     fv_dist           Cox, Gamma      Cox, Log-Normal         RP(P), Gamma
#>       Gamma 0.003 (0.002, 0.003) 0.003 (0.002, 0.003) 0.003 (0.002, 0.003)
#>  Log-Normal 0.002 (0.002, 0.002) 0.002 (0.002, 0.002) 0.002 (0.002, 0.002)
#>     RP(P), Log-Normal
#>  0.003 (0.002, 0.003)
#>  0.002 (0.002, 0.002)
#> 
#> Coverage of nominal 95% confidence interval:
#>     fv_dist           Cox, Gamma      Cox, Log-Normal         RP(P), Gamma
#>       Gamma 0.950 (0.936, 0.964) 0.949 (0.935, 0.963) 0.951 (0.937, 0.964)
#>  Log-Normal 0.941 (0.926, 0.956) 0.942 (0.928, 0.956) 0.943 (0.928, 0.957)
#>     RP(P), Log-Normal
#>  0.950 (0.936, 0.964)
#>  0.943 (0.929, 0.957)

# Printing values only:
print(sms, mcse = NULL)
#> Values are:
#>  Point Estimate
#> 
#>    
#> Parameter: fv 
#> 
#> Bias in point estimate:
#>     fv_dist Cox, Gamma Cox, Log-Normal RP(P), Gamma RP(P), Log-Normal
#>       Gamma    -0.0124          0.2299      -0.0179            0.2347
#>  Log-Normal    -0.1064         -0.0175      -0.1066           -0.0152
#> 
#> Mean squared error:
#>     fv_dist Cox, Gamma Cox, Log-Normal RP(P), Gamma RP(P), Log-Normal
#>       Gamma     0.0203          0.1107       0.0195            0.1145
#>  Log-Normal     0.0287          0.0244       0.0284            0.0248
#> 
#> Coverage of nominal 95% confidence interval:
#>     fv_dist Cox, Gamma Cox, Log-Normal RP(P), Gamma RP(P), Log-Normal
#>       Gamma     0.9201          0.9220       0.9300            0.9030
#>  Log-Normal     0.7503          0.9020       0.7683            0.9280
#> 
#>  -------------------------------------------------------------------------------- 
#>  
#> Parameter: trt 
#> 
#> Bias in point estimate:
#>     fv_dist Cox, Gamma Cox, Log-Normal RP(P), Gamma RP(P), Log-Normal
#>       Gamma    -0.0006         -0.0013      -0.0003           -0.0015
#>  Log-Normal    -0.0006         -0.0014      -0.0006           -0.0016
#> 
#> Mean squared error:
#>     fv_dist Cox, Gamma Cox, Log-Normal RP(P), Gamma RP(P), Log-Normal
#>       Gamma     0.0026          0.0026       0.0026            0.0026
#>  Log-Normal     0.0022          0.0022       0.0022            0.0022
#> 
#> Coverage of nominal 95% confidence interval:
#>     fv_dist Cox, Gamma Cox, Log-Normal RP(P), Gamma RP(P), Log-Normal
#>       Gamma     0.9500          0.9490       0.9506            0.9500
#>  Log-Normal     0.9410          0.9420       0.9428            0.9430
```
