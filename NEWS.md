# rsimsum 0.3.2-9000

Improvements:
* Improved printing of confidence intervals for summary statistics based on Monte Carlo standard errors;
* Added a `description` argument to each `get_data` method, to append a column with a description of each summary statistics exported; defaults to `FALSE`;
* Improved documentation and introductory vignette to clarify several points (#3, @lebebr01);
* Improved plotting vignette to document how to customise plots (#4, @lebebr01).

# rsimsum 0.3.2

`rsimsum` is a small maintenance release:
* Merged pull request #1 from @mllg adapting to new version of the `checkmate` package;
* Fixed a bug where automatic labels in `bar()` and `forest()` were not selected properly.

# rsimsum 0.3.1

Bug fixes:
* `bar()`, `forest()`, `lolly()`, `heat()` now appropriately pick a discrete X (or Y) axis scale for methods (if defined) when the method variable is numeric;
* `simsum()` and `multisimsum()` coerce `methodvar` variable to string format (if specified and not already string);
* fixed typos for empirical standard errors in documentation here and there.

Updated code of conduct (`CONDUCT.md`) and contributing guidelines (`CONTRIBUTING.md`).

Removed dependency on the `tidyverse` package (thanks Mara Averick).

# rsimsum 0.3.0

Bug fixes:
* `pattern()` now appropriately pick a discrete colour scale for methods (if defined) when the method variable is numeric.

New plots are supported:
* `forest()`, for forest plots;
* `bar()`, for bar plots.

Changes to existing functionality:
* the `par` argument of `lolly.multisimsum` is not required anymore; if not provided, plots will be faceted by estimand (as well as any other `by` factor);
* updated _Visualising results from rsimsum_ vignette.

Added `CONTRIBUTING.md` and `CONDUCT.md`.

# rsimsum 0.2.0

Internal housekeeping.

Added S3 methods for `simsum` and `multisimsum` objects to visualise results:
* `lolly()`, for lolly plots;
* `zip()`, for zip plots;
* `heat()`, for heat plots;
* `pattern()`, for scatter plots of estimates vs SEs.

Added a new vignette _Visualising results from rsimsum_ to introduce the abovementioned plots.

Added `x` argument to `simsum` and `multisimsum` to include original dataset as a slot of the returned object.

Added a `miss` function for obtaining basic information on missingness in simulation results. `miss` has methods `print` and `get_data`.

# rsimsum 0.1.0

First submission to CRAN. `rsimsum` can handle:

* simulation studies with a single estimand
* simulation studies with multiple estimands
* simulation studies with multiple methods to compare
* simulation studies with multiple data-generating mechanisms (e.g. 'by' factors)

Summary statistics that can be computed are: bias, empirical standard error, mean squared error, percentage gain in precision relative to a reference method, model-based standard error, coverage, bias-corrected coverage, and power. 

Monte Carlo standard errors for each summary statistic can be computed as well.
