# Custom input values for confidence intervals and true values

## Single Estimand

`rsimsum` supports custom input values for the true value of the
estimand and for confidence intervals limits (used to calculate coverage
probability).

To illustrate this feature, we can use the `tt` dataset (bundled with
`rsimsum`):

``` r

library(rsimsum)
data("tt", package = "rsimsum")
head(tt)
#>        diff       se  conf.low   conf.high       df repno dgm method
#> 1 -2.185467 1.130916 -4.432925  0.06199072 88.00000     1   1      1
#> 2 -3.359683 1.572366 -6.484430 -0.23493506 88.00000     1   2      1
#> 3 -2.185467 1.285290 -4.778318  0.40738411 42.53603     1   1      2
#> 4 -3.359683 2.016465 -7.458611  0.73924596 33.78117     1   2      2
#> 5 -2.989333 1.150093 -5.274900 -0.70376532 88.00000     1   3      1
#> 6 -1.152852 1.368553 -3.872563  1.56685875 88.00000     1   4      1
```

This includes the results of a simulation study assessing robustness of
the t-test when estimating the difference between means. The t-test
assumes a t distribution, hence confidence intervals for the estimated
mean are generally based on the t distribution. See for instance the
example from the t-test documentation
([`?t.test`](https://rdrr.io/r/stats/t.test.html)):

``` r

t.test(extra ~ group, data = sleep)
#> 
#>  Welch Two Sample t-test
#> 
#> data:  extra by group
#> t = -1.8608, df = 17.776, p-value = 0.07939
#> alternative hypothesis: true difference in means between group 1 and group 2 is not equal to 0
#> 95 percent confidence interval:
#>  -3.3654832  0.2054832
#> sample estimates:
#> mean in group 1 mean in group 2 
#>            0.75            2.33
```

We can incorporate custom confidence intervals by passing the name of
two columns in `data` as the `ci.limits` argument:

``` r

s1 <- simsum(data = tt, estvarname = "diff", true = -1, se = "se", ci.limits = c("conf.low", "conf.high"), methodvar = "method", by = "dgm")
#> 'ref' method was not specified, 1 set as the reference
summary(s1, stats = "cover")
#> Values are:
#>  Point Estimate (Monte Carlo Standard Error)
#> 
#> Coverage of nominal 95% confidence interval:
#>  dgm               1               2
#>    1 0.9400 (0.0106) 0.9400 (0.0106)
#>    2 0.8780 (0.0146) 0.9420 (0.0105)
#>    3 0.9380 (0.0108) 0.9500 (0.0097)
#>    4 0.9020 (0.0133) 0.9420 (0.0105)
```

By doing so, we can incorporate different types of confidence intervals
in the analysis of Monte Carlo simulation studies. Compare with the
default setting:

``` r

s2 <- simsum(data = tt, estvarname = "diff", true = -1, se = "se", methodvar = "method", by = "dgm")
#> 'ref' method was not specified, 1 set as the reference
summary(s2, stats = "cover")
#> Values are:
#>  Point Estimate (Monte Carlo Standard Error)
#> 
#> Coverage of nominal 95% confidence interval:
#>  dgm               1               2
#>    1 0.9400 (0.0106) 0.9360 (0.0109)
#>    2 0.8680 (0.0151) 0.9320 (0.0113)
#>    3 0.9380 (0.0108) 0.9420 (0.0105)
#>    4 0.8940 (0.0138) 0.9360 (0.0109)
```

The `ci.limits` is also useful when using non-symmetrical confidence
intervals, e.g. when using bootstrapped confidence intervals.

A pair of values can also be passed to `rsimsum` as the `ci.limits`
argument:

``` r

s3 <- simsum(data = tt, estvarname = "diff", true = -1, se = "se", ci.limits = c(-1.5, -0.5), methodvar = "method", by = "dgm")
#> 'ref' method was not specified, 1 set as the reference
summary(s3, stats = "cover")
#> Values are:
#>  Point Estimate (Monte Carlo Standard Error)
#> 
#> Coverage of nominal 95% confidence interval:
#>  dgm               1               2
#>    1 1.0000 (0.0000) 1.0000 (0.0000)
#>    2 1.0000 (0.0000) 1.0000 (0.0000)
#>    3 1.0000 (0.0000) 1.0000 (0.0000)
#>    4 1.0000 (0.0000) 1.0000 (0.0000)
```

If you have a better example of the utility of this method please get in
touch: I’d love to hear from you!

By default, `simsum` will calculate confidence intervals using
normal-theory, Wald-type intervals. It is possible to use t-based
critical values by providing a column for the (replication-specific)
degrees of freedom (analogously as passing confidence bounds to
`ci.limits`):

``` r

s4 <- simsum(data = tt, estvarname = "diff", true = -1, se = "se", df = "df", methodvar = "method", by = "dgm")
#> 'ref' method was not specified, 1 set as the reference
```

Given that the confidence intervals in (`conf.low`, `conf.high`) are
obtained by using critical values from a t distribution, the results of
`s4` will be equivalent to the results of `s1`:

``` r

all.equal(tidy(s1), tidy(s4))
#> [1] TRUE
```

We can pass a column of values for `true` as well:

``` r

tt$true <- -1
s5 <- simsum(data = tt, estvarname = "diff", true = "true", se = "se", ci.limits = c("conf.low", "conf.high"), methodvar = "method", by = "dgm")
#> 'ref' method was not specified, 1 set as the reference
summary(s5, stats = "cover")
#> Values are:
#>  Point Estimate (Monte Carlo Standard Error)
#> 
#> Coverage of nominal 95% confidence interval:
#>  dgm               1               2
#>    1 0.9400 (0.0106) 0.9400 (0.0106)
#>    2 0.8780 (0.0146) 0.9420 (0.0105)
#>    3 0.9380 (0.0108) 0.9500 (0.0097)
#>    4 0.9020 (0.0133) 0.9420 (0.0105)
```

Compare with the default settings:

``` r

summary(s2, stats = "cover")
#> Values are:
#>  Point Estimate (Monte Carlo Standard Error)
#> 
#> Coverage of nominal 95% confidence interval:
#>  dgm               1               2
#>    1 0.9400 (0.0106) 0.9360 (0.0109)
#>    2 0.8680 (0.0151) 0.9320 (0.0113)
#>    3 0.9380 (0.0108) 0.9420 (0.0105)
#>    4 0.8940 (0.0138) 0.9360 (0.0109)
```

Finally, we could have multiple columns identifying methods as well.
This uses the `MIsim` and `MIsim2` datasets, which are bundled with
{rsimsum}:

``` r

data("MIsim", package = "rsimsum")
data("MIsim2", package = "rsimsum")
head(MIsim)
#> # A tibble: 6 × 4
#>   dataset method      b    se
#>     <dbl> <chr>   <dbl> <dbl>
#> 1       1 CC      0.707 0.147
#> 2       1 MI_T    0.684 0.126
#> 3       1 MI_LOGT 0.712 0.141
#> 4       2 CC      0.349 0.160
#> 5       2 MI_T    0.406 0.141
#> 6       2 MI_LOGT 0.429 0.136
head(MIsim2)
#> # A tibble: 6 × 5
#>   dataset m1    m2         b    se
#>     <dbl> <chr> <chr>  <dbl> <dbl>
#> 1       1 CC    ""     0.707 0.147
#> 2       1 MI    "T"    0.684 0.126
#> 3       1 MI    "LOGT" 0.712 0.141
#> 4       2 CC    ""     0.349 0.160
#> 5       2 MI    "T"    0.406 0.141
#> 6       2 MI    "LOGT" 0.429 0.136
```

The syntax when calling
[`simsum()`](https://ellessenne.github.io/rsimsum/reference/simsum.md)
is pretty much the same:

``` r

s6 <- simsum(data = MIsim, estvarname = "b", true = 0.50, se = "se", methodvar = "method")
#> 'ref' method was not specified, CC set as the reference
s7 <- simsum(data = MIsim2, estvarname = "b", true = 0.50, se = "se", methodvar = c("m1", "m2"))
#> 'ref' method was not specified, CC: set as the reference
```

See the inferred methods:

``` r

print(s6)
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
print(s7)
#> Summary of a simulation study with a single estimand.
#> True value of the estimand: 0.5 
#> 
#> Columns identifying methods: m1, m2 
#>  Unique methods: CC:, MI:LOGT, MI:T 
#> 
#> By factors: none
#> 
#> Monte Carlo standard errors were computed.
```

And of course, the estimated performance measures are the same:

``` r

all.equal(tidy(s6)$est, tidy(s7)$est)
#> [1] TRUE
```

## Multiple Estimands at Once

`multisimsum` can be as flexible as `simsum`. Remember the default
behaviour:

``` r

data("frailty", package = "rsimsum")
ms1 <- multisimsum(
  data = frailty,
  par = "par", true = c(trt = -0.50, fv = 0.75),
  estvarname = "b", se = "se", methodvar = "model",
  by = "fv_dist"
)
#> 'ref' method was not specified, Cox, Gamma set as the reference
summary(ms1, stats = "bias")
#> Values are:
#>  Point Estimate (Monte Carlo Standard Error)
#> 
#>    
#> Parameter: fv 
#> 
#> Bias in point estimate:
#>     fv_dist       Cox, Gamma  Cox, Log-Normal     RP(P), Gamma RP(P), Log-Normal
#>       Gamma -0.0124 (0.0045)  0.2299 (0.0076) -0.0179 (0.0044)   0.2347 (0.0077)
#>  Log-Normal -0.1064 (0.0043) -0.0175 (0.0049) -0.1066 (0.0041)  -0.0152 (0.0050)
#> 
#>  ------------------------------------------------------------------------------------------------------------------------------------------------------ 
#>  
#> Parameter: trt 
#> 
#> Bias in point estimate:
#>     fv_dist       Cox, Gamma  Cox, Log-Normal     RP(P), Gamma RP(P), Log-Normal
#>       Gamma -0.0006 (0.0016) -0.0013 (0.0016) -0.0003 (0.0016)  -0.0015 (0.0016)
#>  Log-Normal -0.0006 (0.0015) -0.0014 (0.0015) -0.0006 (0.0015)  -0.0016 (0.0015)
```

In this example, we pass the true values of each estimand as the named
vector `c(trt = -0.50, fv = 0.75)`.

Say instead we stored the true value of each estimand as a column in our
dataset:

``` r

frailty$true <- ifelse(frailty$par == "trt", -0.50, 0.75)
head(frailty)
#>   i         b        se par    fv_dist             model true
#> 1 1 0.6569546 0.1256964  fv      Gamma        Cox, Gamma 0.75
#> 2 1 0.8396248 0.1663368  fv      Gamma   Cox, Log-Normal 0.75
#> 3 1 0.6583130 0.1260354  fv      Gamma      RP(P), Gamma 0.75
#> 4 1 0.8410503 0.1804898  fv      Gamma RP(P), Log-Normal 0.75
#> 5 1 0.6394722 0.1223808  fv Log-Normal        Cox, Gamma 0.75
#> 6 1 0.7573628 0.1235062  fv Log-Normal   Cox, Log-Normal 0.75
```

With this data structure, we can pass a string value to `multisimsum`
that will identify the `true` column in our dataset:

``` r

ms2 <- multisimsum(
  data = frailty,
  par = "par", true = "true",
  estvarname = "b", se = "se", methodvar = "model",
  by = "fv_dist"
)
#> 'ref' method was not specified, Cox, Gamma set as the reference
summary(ms2, stats = "bias")
#> Values are:
#>  Point Estimate (Monte Carlo Standard Error)
#> 
#>    
#> Parameter: fv 
#> 
#> Bias in point estimate:
#>     fv_dist       Cox, Gamma  Cox, Log-Normal     RP(P), Gamma RP(P), Log-Normal
#>       Gamma -0.0124 (0.0045)  0.2299 (0.0076) -0.0179 (0.0044)   0.2347 (0.0077)
#>  Log-Normal -0.1064 (0.0043) -0.0175 (0.0049) -0.1066 (0.0041)  -0.0152 (0.0050)
#> 
#>  ------------------------------------------------------------------------------------------------------------------------------------------------------ 
#>  
#> Parameter: trt 
#> 
#> Bias in point estimate:
#>     fv_dist       Cox, Gamma  Cox, Log-Normal     RP(P), Gamma RP(P), Log-Normal
#>       Gamma -0.0006 (0.0016) -0.0013 (0.0016) -0.0003 (0.0016)  -0.0015 (0.0016)
#>  Log-Normal -0.0006 (0.0015) -0.0014 (0.0015) -0.0006 (0.0015)  -0.0016 (0.0015)
```

We can confirm that we obtain the same results with the two approaches:

``` r

identical(tidy(ms1), tidy(ms2))
#> [1] TRUE
```

This approach is particularly useful when the true value might vary
across replications (e.g. when it depends on the simulated dataset).

Of course, it can be combined with custom confidence interval limits for
coverage as well:

``` r

frailty$conf.low <- frailty$b - qt(1 - 0.05 / 2, df = 10) * frailty$se
frailty$conf.high <- frailty$b + qt(1 - 0.05 / 2, df = 10) * frailty$se

ms3 <- multisimsum(
  data = frailty,
  par = "par", true = "true",
  estvarname = "b", se = "se", methodvar = "model",
  by = "fv_dist",
  ci.limits = c("conf.low", "conf.high")
)
#> 'ref' method was not specified, Cox, Gamma set as the reference
summary(ms3, stats = "cover")
#> Values are:
#>  Point Estimate (Monte Carlo Standard Error)
#> 
#>    
#> Parameter: fv 
#> 
#> Coverage of nominal 95% confidence interval:
#>     fv_dist      Cox, Gamma Cox, Log-Normal    RP(P), Gamma RP(P), Log-Normal
#>       Gamma 0.9477 (0.0071) 0.9680 (0.0056) 0.9516 (0.0069)   0.9640 (0.0059)
#>  Log-Normal 0.8046 (0.0128) 0.9330 (0.0079) 0.8235 (0.0121)   0.9460 (0.0071)
#> 
#>  ------------------------------------------------------------------------------------------------------------------------------------------------------ 
#>  
#> Parameter: trt 
#> 
#> Coverage of nominal 95% confidence interval:
#>     fv_dist      Cox, Gamma Cox, Log-Normal    RP(P), Gamma RP(P), Log-Normal
#>       Gamma 0.9730 (0.0051) 0.9710 (0.0053) 0.9732 (0.0052)   0.9710 (0.0053)
#>  Log-Normal 0.9710 (0.0053) 0.9690 (0.0055) 0.9719 (0.0052)   0.9690 (0.0055)
```

This will be completely different than before:

``` r

summary(ms2, stats = "cover")
#> Values are:
#>  Point Estimate (Monte Carlo Standard Error)
#> 
#>    
#> Parameter: fv 
#> 
#> Coverage of nominal 95% confidence interval:
#>     fv_dist      Cox, Gamma Cox, Log-Normal    RP(P), Gamma RP(P), Log-Normal
#>       Gamma 0.9201 (0.0087) 0.9220 (0.0085) 0.9300 (0.0082)   0.9030 (0.0094)
#>  Log-Normal 0.7503 (0.0140) 0.9020 (0.0094) 0.7683 (0.0134)   0.9280 (0.0082)
#> 
#>  ------------------------------------------------------------------------------------------------------------------------------------------------------ 
#>  
#> Parameter: trt 
#> 
#> Coverage of nominal 95% confidence interval:
#>     fv_dist      Cox, Gamma Cox, Log-Normal    RP(P), Gamma RP(P), Log-Normal
#>       Gamma 0.9500 (0.0069) 0.9490 (0.0070) 0.9506 (0.0070)   0.9500 (0.0069)
#>  Log-Normal 0.9410 (0.0075) 0.9420 (0.0074) 0.9428 (0.0074)   0.9430 (0.0073)
```

Multiple columns identifying methods are supported with
[`multisimsum()`](https://ellessenne.github.io/rsimsum/reference/multisimsum.md)
as well; examples are omitted here, but it works analogously as with
[`simsum()`](https://ellessenne.github.io/rsimsum/reference/simsum.md).
