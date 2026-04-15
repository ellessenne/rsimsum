# Analyses of simulation studies including Monte Carlo error

`simsum()` computes performance measures for simulation studies in which
each simulated data set yields point estimates by one or more analysis
methods. Bias, relative bias, empirical standard error and precision
relative to a reference method can be computed for each method. If, in
addition, model-based standard errors are available then `simsum()` can
compute the average model-based standard error, the relative error in
the model-based standard error, the coverage of nominal confidence
intervals, the coverage under the assumption that there is no bias
(bias-eliminated coverage), and the power to reject a null hypothesis.
Monte Carlo errors are available for all estimated quantities.

## Usage

``` r
simsum(
  data,
  estvarname,
  se = NULL,
  true = NULL,
  methodvar = NULL,
  ref = NULL,
  by = NULL,
  ci.limits = NULL,
  df = NULL,
  dropbig = FALSE,
  x = FALSE,
  control = list()
)
```

## Arguments

- data:

  A `data.frame` in which variable names are interpreted. It has to be
  in tidy format, e.g. each variable forms a column and each observation
  forms a row.

- estvarname:

  The name of the variable containing the point estimates. Note that
  some column names are forbidden: these are listed below in the
  *Details* section.

- se:

  The name of the variable containing the standard errors of the point
  estimates. Note that some column names are forbidden: these are listed
  below in the *Details* section.

- true:

  The true value of the parameter; this is used in calculations of bias,
  relative bias, coverage, and mean squared error and is required
  whenever these performance measures are requested. `true` can be a
  numeric value or a string that identifies a column in `data`. In the
  former setting, `simsum` will assume the same value for all
  replications; conversely, each replication will use a distinct value
  for `true` as identified by each row of `data`. See
  [`vignette("E-custom-inputs", package = "rsimsum")`](https://ellessenne.github.io/rsimsum/articles/E-custom-inputs.md)
  for more details. Note that some column names are forbidden: these are
  listed below in the *Details* section.

- methodvar:

  The name of the variable containing the methods to compare. For
  instance, methods could be the models compared within a simulation
  study. Can be `NULL`. If a vector of column names is passed to
  `simsum()`, those columns will be combined into a single column named
  `:methodvar` using the
  [`base::interaction()`](https://rdrr.io/r/base/interaction.html)
  function before computing all performance measures. Note that some
  column names are forbidden: these are listed below in the *Details*
  section.

- ref:

  Specifies the reference method against which relative precision will
  be calculated. Only useful if `methodvar` is specified.

- by:

  A vector of variable names to compute performance measures by a list
  of factors. Factors listed here are the (potentially several)
  data-generating mechanisms used to simulate data under different
  scenarios (e.g. sample size, true distribution of a variable, etc.).
  Can be `NULL`. Note that some column names are forbidden: these are
  listed below in the *Details* section.

- ci.limits:

  Can be used to specify the limits (lower and upper) of confidence
  intervals used to calculate coverage and bias-eliminated coverage.
  Useful for non-Wald type estimators (e.g. bootstrap). Defaults to
  `NULL`, where Wald-type confidence intervals based on the provided SEs
  are calculated for coverage; otherwise, it can be a numeric vector
  (for fixed confidence intervals) or a vector of strings that identify
  columns in `data` with replication-specific lower and upper limits.
  See
  [`vignette("E-custom-inputs", package = "rsimsum")`](https://ellessenne.github.io/rsimsum/articles/E-custom-inputs.md)
  for more details. Note that some column names are forbidden: these are
  listed below in the *Details* section.

- df:

  Can be used to specify that a column containing the
  replication-specific number of degrees of freedom that will be used to
  calculate confidence intervals for coverage (and bias-eliminated
  coverage) assuming t-distributed critical values (rather than normal
  theory intervals). See
  [`vignette("E-custom-inputs", package = "rsimsum")`](https://ellessenne.github.io/rsimsum/articles/E-custom-inputs.md)
  for more details. Note that some column names are forbidden: these are
  listed below in the *Details* section.

- dropbig:

  Specifies that point estimates or standard errors beyond the maximum
  acceptable values should be dropped. Defaults to `FALSE`.

- x:

  Set to `TRUE` to include the `data` argument used to calculate summary
  statistics (i.e. after pre-processing the input dataset e.g. removing
  values deemed too large via the `dropbig` argument) as a slot. Calling
  `simsum` with `x = TRUE` is required to produce zipper plots. The
  downside is that the size of the returned object increases
  considerably, therefore it is set to `FALSE` by default.

- control:

  A list of parameters that control the behaviour of `simsum`. Possible
  values are:

  - `mcse`, whether to calculate Monte Carlo standard errors. Defaults
    to `TRUE`;

  - `level`, the significance level used for coverage, bias-eliminated
    coverage, and power. Defaults to 0.95;

  - `power_df`, whether to use robust critical values from a t
    distribution with `power_df` degrees of freedom when calculating
    power. Defaults to `NULL`, in which case a Gaussian distribution is
    used;

  - `na.rm`, whether to remove point estimates or standard errors where
    either (or both) is missing. Defaults to `TRUE`;

  - `char.sep`, a character utilised when splitting the input dataset
    `data`. Generally, this should not be changed;

  - `dropbig.max`, specifies the maximum acceptable absolute value of
    the point estimates, after standardisation. Defaults to 10;

  - `dropbig.semax`, specifies the maximum acceptable absolute value of
    the standard error, after standardisation. Defaults to 100

  - `dropbig.robust`, specifies whether to use robust standardisation
    (using median and inter-quartile range) rather than normal
    standardisation (using mean and standard deviation). Defaults to
    `TRUE`, in which case robust standardisation will be used for
    `dropbig`.

## Value

An object of class `simsum`.

## Details

The following names are not allowed for any column in `data` that is
passed to `simsum()`: `stat`, `est`, `mcse`, `lower`, `upper`,
`:methodvar`, `:true`.

## References

White, I.R. 2010. simsum: Analyses of simulation studies including Monte
Carlo error. The Stata Journal 10(3): 369-385.
<https://www.stata-journal.com/article.html?article=st0200>

Morris, T.P., White, I.R. and Crowther, M.J. 2019. *Using simulation
studies to evaluate statistical methods*. Statistics in Medicine,
[doi:10.1002/sim.8086](https://doi.org/10.1002/sim.8086)

Gasparini, A. 2018. rsimsum: Summarise results from Monte Carlo
simulation studies. Journal of Open Source Software 3(26):739,
[doi:10.21105/joss.00739](https://doi.org/10.21105/joss.00739)

## Examples

``` r
data("MIsim", package = "rsimsum")
s <- simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method", ref = "CC")
# If 'ref' is not specified, the reference method is inferred
s <- simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method")
#> 'ref' method was not specified, CC set as the reference
```
