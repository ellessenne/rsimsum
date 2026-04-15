# Example of a simulation study on missing data

A dataset from a simulation study comparing different ways to handle
missing covariates when fitting a Cox model (White and Royston, 2009).
One thousand datasets were simulated, each containing normally
distributed covariates \\x\\ and \\z\\ and time-to-event outcome. Both
covariates have 20% of their values deleted independently of all other
variables so the data became missing completely at random (Little and
Rubin, 2002). Each simulated dataset was analysed in three ways. A Cox
model was fit to the complete cases (`CC`). Then two methods of multiple
imputation using chained equations (van Buuren, Boshuizen, and Knook,
1999) were used. The `MI_LOGT` method multiply imputes the missing
values of \\x\\ and \\z\\ with the outcome included as \\\log (t)\\ and
\\d\\, where \\t\\ is the survival time and \\d\\ is the event
indicator. The `MI_T` method is the same except that \\\log (t)\\ is
replaced by \\t\\ in the imputation model. The results are stored in
long format.

## Usage

``` r
MIsim

MIsim2
```

## Format

A data frame with 3,000 rows and 4 variables:

- `dataset` Simulated dataset number.

- `method` Method used (`CC`, `MI_LOGT` or `MI_T`).

- `b` Point estimate.

- `se` Standard error of the point estimate.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with
3000 rows and 5 columns.

## Note

`MIsim2` is a version of the same dataset with the `method` column split
into two columns, `m1` and `m2`.

## References

White, I.R., and P. Royston. 2009. Imputing missing covariate values for
the Cox model. Statistics in Medicine 28(15):1982-1998
[doi:10.1002/sim.3618](https://doi.org/10.1002/sim.3618)

## Examples

``` r
data("MIsim", package = "rsimsum")
data("MIsim2", package = "rsimsum")
```
