# Example of a simulation study on the t-test

A dataset from a simulation study with 4 data-generating mechanisms,
useful to illustrate custom input of confidence intervals to calculate
coverage probability. This simulation study aims to compare the t-test
assuming pooled or unpooled variance in violation (or not) of the t-test
assumptions: normality of data, and equality (or not) or variance
between groups. The true value of the difference between groups is -1.

## Usage

``` r
tt
```

## Format

A data frame with 4,000 rows and 8 variables:

- `diff` The difference in mean between groups estimated by the t-test;

- `se` Standard error of the estimated difference;

- `conf.low`, `conf.high` Confidence interval for the difference in mean
  as reported by the t-test;

- `df` The number of degrees of freedom assumed by the t-test;

- `repno` Identifies each replication, between 1 and 500;

- `dgm` Identifies each data-generating mechanism: 1 corresponds to
  normal data with equal variance between the groups, 2 is normal data
  with unequal variance, 3 and 4 are skewed data (simulated from a Gamma
  distribution) with equal and unequal variance between groups,
  respectively;

- `method` Analysis method: 1 represents the t-test with pooled
  variance, while 2 represents the t-test with unpooled variance.

## Note

Further details on this simulation study can be found in the R script
used to generate this dataset, available on GitHub:
<https://github.com/ellessenne/rsimsum/blob/master/data-raw/tt-data.R>

## Examples

``` r
data("tt", package = "rsimsum")
```
