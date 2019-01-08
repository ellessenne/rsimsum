# rsimsum 0.4.0-9000

TO DO:

* revamp plotting methods (e.g. use autoplot and so on)
* vignettes need to be updated to reflect all changes from the refactoring
* code coverage, eventually, once all of the above is done
* anything that comes up in the meanwhile
* check docs
* rebuild pkgdown website
--> targeting rsimsum 0.5.0 to go on CRAN ASAP

# rsimsum 0.4.0

`rsimsum` 0.4.0 is a large refactoring of `rsimsum`. 
There are several improvements and breaking changes, outlined below.

### Improvements

* `rsimsum` is more robust to using factor variables (e.g. as `methodvar` or `by` factor), with ordering that will be preserved if defined in the dataset passed to `simsum` (or `multisimsum`);
* Confidence intervals based on Monte Carlo standard errors can be now computed using quantiles from a t distribution; see `help(summary.simsum)` for more details;
* Added comparison with results from Stata's `simsum` for testing purposes - differences are negligible, and there are some calculations in `simsum` that are wrong (already reported). Most differences can be attributed to calculations (and conversions, for comparison) on different scales.

### Breaking changes

* The syntax of `simsum` and `multisimsum` has been slightly changed, with some arguments being removed and others being moved to a `control` list with several tuning parameters. Please check the updated examples for more details;
* `dropbig` is no longer an S3 method for `simsum` and `multisimsum` objects. Now, `dropbig` is an exported function that can be used to identify rows of the input `data.frame` that would be dropped by `simsum` (or `multisimsum`);
* Point estimates and standard errors dropped by `simsum` (or `multisimsum`) when `dropbig = TRUE)` are no longer included in the returned object; therefore, the S3 method `miss` has been removed;
* `get_data` is no longer an S3 method, but still requires an object of class `simsum`, `summary.simsum`, `multisimsum`, or `summary.multisimsum` to be passed as input;
* All plotting methods have been removed in preparation of a complete overhaul planned for `rsimsum` 0.5.0.

# rsimsum 0.3.5

### Breaking changes

* The `zip` method has been renamed to `zipper()` to avoid name collision with `utils::zip()`.

# rsimsum 0.3.4

* Added ability to define custom confidence interval limits for calculating coverage via the `ci.limits` argument (#6, @MvanSmeden). This functionality is to be considered experimental, hence feedback would be much appreciated;
* Updated _Simulating a simulation study_ vignette and therefore the `relhaz` dataset bundled with `rsimsum`.

# rsimsum 0.3.3

`rsimsum` 0.3.3 focuses on improving the documentation of the package.

Improvements:
* Improved printing of confidence intervals for summary statistics based on Monte Carlo standard errors;
* Added a `description` argument to each `get_data` method, to append a column with a description of each summary statistics exported; defaults to `FALSE`;
* Improved documentation and introductory vignette to clarify several points (#3, @lebebr01);
* Improved plotting vignette to document how to customise plots (#4, @lebebr01).

New:
* Added CITATION file with references to paper in JOSS.

# rsimsum 0.3.2

`rsimsum` 0.3.2 is a small maintenance release:
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
