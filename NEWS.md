# rsimsum 0.8.0

### Improvements:

* Added new argument `zoom` to `autoplot` methods: it is now possible to _zoom_ on the top x% of a zip plot to improve readability;

* Added a new example dataset from a toy simulation study assessing the robustness of the t-test. See `?"tt"` for more details;

* The `true` argument of `rsimsum` and `multisimsum` now accepts a string that identifies a column in `data`. This is especially useful in settings where the true value varies across replications, e.g. when it depends on characteristics of the simulated data. See `vignette("E-custom-inputs", package = "rsimsum")` for more details and examples;

* Analogously, the `ci.limits` argument now accepts a vector of strings that identifies lower and upper limits for custom-defined confidence intervals from columns in `data`. Once again, more details are included in `vignette("E-custom-inputs", package = "rsimsum")`;

* `rsimsum` now correctly uses `inherits(obj, "someclass")` instead of `class(obj) == "someclass"` (#20).

# rsimsum 0.7.1

* `autoplot` supports two new visualisations: contour plots and hexbin plots, for either point estimates or standard errors. They can be obtained by selecting the argument `type = "est_density"`, `type = "se_density"`, `type = "est_hex"`, or `type = "se_hex"`.

# rsimsum 0.7.0

### Improvements:

* Passing the true value of an estimand (`true` argument) is no longer required; if `true` is not passed to `simsum` or `multisimsum`, bias, coverage, and mean squared error are not computed;

* Passing estimated standard errors per replication (`se` argument) is no longer required; if so, average and median variances, model-based standard errors, relative error, coverage probability, bias-eliminated coverage probability, power are not computed.

# rsimsum 0.6.2

### Bug fixes:

* Fixed bug introduced in `rsimsum` 0.6.1 (average and median variances were not printed).

# rsimsum 0.6.1

### Bug fixes:

* Fixed labelling bug in zipper plots (thanks to @syriop-elisa for reporting it);

* Clarified that `simsum` and `multisimsum` report average (or median) estimated variances, not standard errors (thanks to Ian R. White for reporting this).

# rsimsum 0.6.0

### Improvements:

* Implemented fully automated nested loop plots for simulation studies with several data-generating mechanisms: `autoplot(object, type = "nlp")`;

* Added `data("nlp", package = "rsimsum")`, a dataset from a simulation study with 150 data-generating. This is particularly useful to illustrate nested loop plots;

* Added a new vignette on nested loop plots;

* Improved ordering of vignettes.

### Bug fixes:

* Updated unquoting for compatibility with `rlang` 0.4.0;

* Fixed missing details and options in the documentation of `autoplot.multisimsum` and `autoplot.summary.multisimsum`.

# rsimsum 0.5.2

### Bug fixes:

* Fixed labelling when facetting for some plot types, now all defaults to `ggplot2::label_both` for 'by' factors (when included).

# rsimsum 0.5.1

### Bug fixes:

* Fixed calculations for "Relative % increase in precision" (thanks to Ian R. White for reporting this).

# rsimsum 0.5.0

### Improvements:

* Implemented `autoplot` method for `multisimsum` and `summary.multisimsum` objects;
* Implemented heat plot types for both `simsum` and `multisimsum` objects;
* All `autoplot` methods pick the value of `true` passed to `simsum`, `multisimsum` when inferring the target value if `stats = (thetamean, thetamedian)` and `target = NULL`. In plain English, the true value of the estimand is picked as target value when plotting the mean (or median) of the estimated value;
* Updated vignettes and references;
* Updated `pkgdown` website, published at https://ellessenne.github.io/rsimsum/;
* Improved code coverage.

### Bug fixes:

* Fixed a bug in `autoplot` caused by premature slicing of `by` arguments, where no `by` arguments were included.

# rsimsum 0.4.2

Implemented `autoplot` method for `simsum` and `summary.simsum` objects; when calling `autoplot` on `summary.simsum` objects, confidence intervals based on Monte Carlo standard errors will be included as well (if sensible).

Supported plot types are:

* forest plot of estimated summary statistics;
* lolly plot of summary statistics;
* zip plot for coverage probability;
* scatter plot of methods-wise comparison (e.g. X vs Y) of point estimates and standard errors, per replication;
* same as the above, but implemented as a Bland-Altman type plot;
* ridgeline plot of estimates, standard errors to compare the distribution of estimates, standard errors by method.

Several options to customise the behaviour of `autoplot`, see `?autoplot.simsum` and `?autoplot.summary.simsum` for further details.

# rsimsum 0.4.1

Fixed a bug in `dropbig` and related internal function that was returning standardised values instead of actual observed values.

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
* the `par` argument of `lolly.multisimsum` is now not required; if not provided, plots will be faceted by estimand (as well as any other `by` factor);
* updated _Visualising results from rsimsum_ vignette.

Added `CONTRIBUTING.md` and `CONDUCT.md`.

# rsimsum 0.2.0

Internal housekeeping.

Added S3 methods for `simsum` and `multisimsum` objects to visualise results:
* `lolly()`, for lolly plots;
* `zip()`, for zip plots;
* `heat()`, for heat plots;
* `pattern()`, for scatter plots of estimates vs SEs.

Added a new vignette _Visualising results from rsimsum_ to introduce the above-mentioned plots.

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
