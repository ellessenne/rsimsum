# print.multisimsum

Print method for multisimsum objects

## Usage

``` r
# S3 method for class 'multisimsum'
print(x, ...)
```

## Arguments

- x:

  An object of class `multisimsum`.

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
ms
#> 
#> Estimands variable: par 
#>  Unique estimands: fv, trt 
#>  True values: fv = -0.5, trt = 0.75 
#> 
#> Method variable: model 
#>  Unique methods: Cox, Gamma, Cox, Log-Normal, RP(P), Gamma, RP(P), Log-Normal 
#>  Reference method: Cox, Gamma 
#> 
#> By factors: fv_dist 
#> 
#> Monte Carlo standard errors were computed.

data("frailty", package = "rsimsum")
frailty$true <- ifelse(frailty$par == "trt", -0.50, 0.75)
ms <- multisimsum(data = frailty, par = "par", estvarname = "b", true = "true")
ms
#> 
#> Estimands variable: par 
#>  Unique estimands: fv, trt 
#>  True values from column 'true' 
#> 
#> Method variable: none
#> 
#> By factors: none
#> 
#> Monte Carlo standard errors were computed.
```
