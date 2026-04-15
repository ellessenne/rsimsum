# Create 'kable's

Create tables in LaTeX, HTML, Markdown, or reStructuredText from objects
of class `simsum`, `summary.simsum`, `multisimsum`,
`summary.multisimsum`.

## Usage

``` r
# S3 method for class 'simsum'
kable(x, stats = NULL, digits = max(3, getOption("digits") - 3), ...)

# S3 method for class 'summary.simsum'
kable(x, stats = NULL, digits = max(3, getOption("digits") - 3), ...)

# S3 method for class 'multisimsum'
kable(x, stats = NULL, digits = max(3, getOption("digits") - 3), ...)

# S3 method for class 'summary.multisimsum'
kable(x, stats = NULL, digits = max(3, getOption("digits") - 3), ...)

kable(x, ...)
```

## Arguments

- x:

  An object of class `simsum`, `summary.simsum`, `multisimsum`,
  `summary.multisimsum`;

- stats:

  Summary statistics to include. See
  [`tidy()`](https://generics.r-lib.org/reference/tidy.html) for more
  details;

- digits:

  Maximum number of digits for numeric columns;

- ...:

  Further arguments passed to
  [`knitr::kable()`](https://rdrr.io/pkg/knitr/man/kable.html).

## See also

[`knitr::kable()`](https://rdrr.io/pkg/knitr/man/kable.html)
