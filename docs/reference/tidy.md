# Turn an object into a tidy dataset

Extract a tidy dataset with results from an object of class `simsum`,
`summary.simsum`, `multisimsum`, or `summary.multisimsum`.

## Usage

``` r
# S3 method for class 'simsum'
tidy(x, stats = NULL, ...)

# S3 method for class 'summary.simsum'
tidy(x, stats = NULL, ...)

# S3 method for class 'multisimsum'
tidy(x, stats = NULL, ...)

# S3 method for class 'summary.multisimsum'
tidy(x, stats = NULL, ...)
```

## Arguments

- x:

  An object of class `simsum`.

- stats:

  Summary statistics to include; can be a scalar value or a vector.
  Possible choices are:

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

  - `becover`, bias-eliminated coverage of a nominal `level`\\

  - `power`, power of a (1 - `level`)\\ Defaults to `NULL`, in which
    case all summary statistics are returned.

- ...:

  Ignored.

## Value

A `data.frame` containing summary statistics from a simulation study.

## Examples

``` r
data(MIsim)
x <- simsum(
  data = MIsim, estvarname = "b", true = 0.5, se = "se",
  methodvar = "method"
)
#> 'ref' method was not specified, CC set as the reference
tidy(x)
#>           stat           est         mcse  method
#> 1         nsim  1.000000e+03           NA      CC
#> 2    thetamean  5.167662e-01           NA      CC
#> 3  thetamedian  5.069935e-01           NA      CC
#> 4      se2mean  2.163731e-02           NA      CC
#> 5    se2median  2.114245e-02           NA      CC
#> 6         bias  1.676616e-02 4.778676e-03      CC
#> 7        rbias  3.353232e-02 9.557351e-03      CC
#> 8        empse  1.511150e-01 3.380725e-03      CC
#> 9          mse  2.309401e-02 1.133839e-03      CC
#> 10     relprec  0.000000e+00 9.429038e-08      CC
#> 11     modelse  1.470963e-01 5.274099e-04      CC
#> 12    relerror -2.659384e+00 2.205482e+00      CC
#> 13       cover  9.430000e-01 7.331507e-03      CC
#> 14     becover  9.400000e-01 7.509993e-03      CC
#> 15       power  9.460000e-01 7.147307e-03      CC
#> 16        nsim  1.000000e+03           NA MI_LOGT
#> 17   thetamean  5.009231e-01           NA MI_LOGT
#> 18 thetamedian  4.969223e-01           NA MI_LOGT
#> 19     se2mean  1.820915e-02           NA MI_LOGT
#> 20   se2median  1.721574e-02           NA MI_LOGT
#> 21        bias  9.230987e-04 4.174410e-03 MI_LOGT
#> 22       rbias  1.846197e-03 8.348820e-03 MI_LOGT
#> 23       empse  1.320064e-01 2.953231e-03 MI_LOGT
#> 24         mse  1.740913e-02 8.812805e-04 MI_LOGT
#> 25     relprec  3.104634e+01 3.937473e+00 MI_LOGT
#> 26     modelse  1.349413e-01 6.046041e-04 MI_LOGT
#> 27    relerror  2.223259e+00 2.332338e+00 MI_LOGT
#> 28       cover  9.490000e-01 6.956939e-03 MI_LOGT
#> 29     becover  9.490000e-01 6.956939e-03 MI_LOGT
#> 30       power  9.690000e-01 5.480785e-03 MI_LOGT
#> 31        nsim  1.000000e+03           NA    MI_T
#> 32   thetamean  4.988092e-01           NA    MI_T
#> 33 thetamedian  4.939111e-01           NA    MI_T
#> 34     se2mean  1.791169e-02           NA    MI_T
#> 35   se2median  1.693191e-02           NA    MI_T
#> 36        bias -1.190835e-03 4.250977e-03    MI_T
#> 37       rbias -2.381670e-03 8.501953e-03    MI_T
#> 38       empse  1.344277e-01 3.007399e-03    MI_T
#> 39         mse  1.805415e-02 9.112249e-04    MI_T
#> 40     relprec  2.636816e+01 3.842379e+00    MI_T
#> 41     modelse  1.338346e-01 5.856362e-04    MI_T
#> 42    relerror -4.412233e-01 2.269522e+00    MI_T
#> 43       cover  9.430000e-01 7.331507e-03    MI_T
#> 44     becover  9.430000e-01 7.331507e-03    MI_T
#> 45       power  9.630000e-01 5.969171e-03    MI_T

# Extracting only bias and coverage:
tidy(x, stats = c("bias", "cover"))
#>    stat           est        mcse  method
#> 1  bias  0.0167661608 0.004778676      CC
#> 2 cover  0.9430000000 0.007331507      CC
#> 3  bias  0.0009230987 0.004174410 MI_LOGT
#> 4 cover  0.9490000000 0.006956939 MI_LOGT
#> 5  bias -0.0011908351 0.004250977    MI_T
#> 6 cover  0.9430000000 0.007331507    MI_T

xs <- summary(x)
tidy(xs)
#>           stat           est         mcse  method         lower        upper
#> 1         nsim  1.000000e+03           NA      CC            NA           NA
#> 2    thetamean  5.167662e-01           NA      CC            NA           NA
#> 3  thetamedian  5.069935e-01           NA      CC            NA           NA
#> 4      se2mean  2.163731e-02           NA      CC            NA           NA
#> 5    se2median  2.114245e-02           NA      CC            NA           NA
#> 6         bias  1.676616e-02 4.778676e-03      CC  7.400129e-03 2.613219e-02
#> 7        rbias  3.353232e-02 9.557351e-03      CC  1.480026e-02 5.226439e-02
#> 8        empse  1.511150e-01 3.380725e-03      CC  1.444889e-01 1.577411e-01
#> 9          mse  2.309401e-02 1.133839e-03      CC  2.087173e-02 2.531629e-02
#> 10     relprec  0.000000e+00 9.429038e-08      CC -1.848057e-07 1.848057e-07
#> 11     modelse  1.470963e-01 5.274099e-04      CC  1.460626e-01 1.481300e-01
#> 12    relerror -2.659384e+00 2.205482e+00      CC -6.982049e+00 1.663281e+00
#> 13       cover  9.430000e-01 7.331507e-03      CC  9.286305e-01 9.573695e-01
#> 14     becover  9.400000e-01 7.509993e-03      CC  9.252807e-01 9.547193e-01
#> 15       power  9.460000e-01 7.147307e-03      CC  9.319915e-01 9.600085e-01
#> 16        nsim  1.000000e+03           NA MI_LOGT            NA           NA
#> 17   thetamean  5.009231e-01           NA MI_LOGT            NA           NA
#> 18 thetamedian  4.969223e-01           NA MI_LOGT            NA           NA
#> 19     se2mean  1.820915e-02           NA MI_LOGT            NA           NA
#> 20   se2median  1.721574e-02           NA MI_LOGT            NA           NA
#> 21        bias  9.230987e-04 4.174410e-03 MI_LOGT -7.258595e-03 9.104792e-03
#> 22       rbias  1.846197e-03 8.348820e-03 MI_LOGT -1.451719e-02 1.820958e-02
#> 23       empse  1.320064e-01 2.953231e-03 MI_LOGT  1.262182e-01 1.377947e-01
#> 24         mse  1.740913e-02 8.812805e-04 MI_LOGT  1.568185e-02 1.913640e-02
#> 25     relprec  3.104634e+01 3.937473e+00 MI_LOGT  2.332904e+01 3.876365e+01
#> 26     modelse  1.349413e-01 6.046041e-04 MI_LOGT  1.337563e-01 1.361263e-01
#> 27    relerror  2.223259e+00 2.332338e+00 MI_LOGT -2.348040e+00 6.794558e+00
#> 28       cover  9.490000e-01 6.956939e-03 MI_LOGT  9.353647e-01 9.626353e-01
#> 29     becover  9.490000e-01 6.956939e-03 MI_LOGT  9.353647e-01 9.626353e-01
#> 30       power  9.690000e-01 5.480785e-03 MI_LOGT  9.582579e-01 9.797421e-01
#> 31        nsim  1.000000e+03           NA    MI_T            NA           NA
#> 32   thetamean  4.988092e-01           NA    MI_T            NA           NA
#> 33 thetamedian  4.939111e-01           NA    MI_T            NA           NA
#> 34     se2mean  1.791169e-02           NA    MI_T            NA           NA
#> 35   se2median  1.693191e-02           NA    MI_T            NA           NA
#> 36        bias -1.190835e-03 4.250977e-03    MI_T -9.522596e-03 7.140926e-03
#> 37       rbias -2.381670e-03 8.501953e-03    MI_T -1.904519e-02 1.428185e-02
#> 38       empse  1.344277e-01 3.007399e-03    MI_T  1.285333e-01 1.403221e-01
#> 39         mse  1.805415e-02 9.112249e-04    MI_T  1.626818e-02 1.984012e-02
#> 40     relprec  2.636816e+01 3.842379e+00    MI_T  1.883724e+01 3.389909e+01
#> 41     modelse  1.338346e-01 5.856362e-04    MI_T  1.326867e-01 1.349824e-01
#> 42    relerror -4.412233e-01 2.269522e+00    MI_T -4.889404e+00 4.006957e+00
#> 43       cover  9.430000e-01 7.331507e-03    MI_T  9.286305e-01 9.573695e-01
#> 44     becover  9.430000e-01 7.331507e-03    MI_T  9.286305e-01 9.573695e-01
#> 45       power  9.630000e-01 5.969171e-03    MI_T  9.513006e-01 9.746994e-01
```
