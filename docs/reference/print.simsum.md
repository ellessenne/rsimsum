# print.simsum

Print method for simsum objects

## Usage

``` r
# S3 method for class 'simsum'
print(x, ...)
```

## Arguments

- x:

  An object of class `simsum`.

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
x
#> Summary of a simulation study with a single estimand.
#> True value of the estimand: 0.5 
#> 
#> Method variable: method 
#>  Unique methods: CC, MI_LOGT, MI_T 
#>  Reference method: CC 
#> 
#> By factors: none
#> 
#> Monte Carlo standard errors were computed.

MIsim$true <- 0.5
x <- simsum(data = MIsim, estvarname = "b", true = "true", se = "se")
x
#> Summary of a simulation study with a single estimand.
#> True value of the estimand from column 'true' 
#> 
#> Method variable: none
#> 
#> By factors: none
#> 
#> Monte Carlo standard errors were computed.
```
