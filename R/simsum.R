#' @title Analyses of simulation studies including Monte Carlo error
#' @description `simsum()` computes performance measures for simulation studies in which each simulated data set yields point estimates by one or more analysis methods.
#' Bias, relative bias, empirical standard error and precision relative to a reference method can be computed for each method.
#' If, in addition, model-based standard errors are available then `simsum()` can compute the average model-based standard error, the relative error in the model-based standard error, the coverage of nominal confidence intervals, the coverage under the assumption that there is no bias (bias-eliminated coverage), and the power to reject a null hypothesis.
#' Monte Carlo errors are available for all estimated quantities.
#' @param data A `data.frame` in which variable names are interpreted.
#' It has to be in tidy format, e.g. each variable forms a column and each observation forms a row.
#' @param estvarname The name of the variable containing the point estimates.
#' Note that some column names are forbidden: these are listed below in the _Details_ section.
#' @param se The name of the variable containing the standard errors of the point estimates.
#' Note that some column names are forbidden: these are listed below in the _Details_ section.
#' @param true The true value of the parameter; this is used in calculations of bias, relative bias, coverage, and mean squared error and is required whenever these performance measures are requested.
#' `true` can be a numeric value or a string that identifies a column in `data`.
#' In the former setting, `simsum` will assume the same value for all replications; conversely, each replication will use a distinct value for `true` as identified by each row of `data`.
#' See `vignette("E-custom-inputs", package = "rsimsum")` for more details.
#' Note that some column names are forbidden: these are listed below in the _Details_ section.
#' @param methodvar The name of the variable containing the methods to compare.
#' For instance, methods could be the models compared within a simulation study.
#' Can be `NULL`.
#' If a vector of column names is passed to `simsum()`, those columns will be combined into a single column named `:methodvar` using the [base::interaction()] function before computing all performance measures.
#' Note that some column names are forbidden: these are listed below in the _Details_ section.
#' @param ref Specifies the reference method against which relative precision will be calculated.
#' Only useful if `methodvar` is specified.
#' @param by A vector of variable names to compute performance measures by a list of factors. Factors listed here are the (potentially several) data-generating mechanisms used to simulate data under different scenarios (e.g. sample size, true distribution of a variable, etc.).
#' Can be `NULL`.
#' Note that some column names are forbidden: these are listed below in the _Details_ section.
#' @param ci.limits Can be used to specify the limits (lower and upper) of confidence intervals used to calculate coverage and bias-eliminated coverage.
#' Useful for non-Wald type estimators (e.g. bootstrap).
#' Defaults to `NULL`, where Wald-type confidence intervals based on the provided SEs are calculated for coverage; otherwise, it can be a numeric vector (for fixed confidence intervals) or a vector of strings that identify columns in `data` with replication-specific lower and upper limits.
#' See `vignette("E-custom-inputs", package = "rsimsum")` for more details.
#' Note that some column names are forbidden: these are listed below in the _Details_ section.
#' @param df Can be used to specify that a column containing the replication-specific number of degrees of freedom that will be used to calculate confidence intervals for coverage (and bias-eliminated coverage) assuming t-distributed critical values (rather than normal theory intervals).
#' See `vignette("E-custom-inputs", package = "rsimsum")` for more details.
#' Note that some column names are forbidden: these are listed below in the _Details_ section.
#' @param dropbig Specifies that point estimates or standard errors beyond the maximum acceptable values should be dropped. Defaults to `FALSE`.
#' @param x Set to `TRUE` to include the `data` argument used to calculate summary statistics (i.e. after pre-processing the input dataset e.g. removing values deemed too large via the `dropbig` argument) as a slot. Calling `simsum` with `x = TRUE` is required to produce zipper plots. The downside is that the size of the returned object increases considerably, therefore it is set to `FALSE` by default.
#' @param control A list of parameters that control the behaviour of `simsum`.
#' Possible values are:
#' * `mcse`, whether to calculate Monte Carlo standard errors. Defaults to `TRUE`;
#' * `level`, the significance level used for coverage, bias-eliminated coverage, and power. Defaults to 0.95;
#' * `power_df`, whether to use robust critical values from a t distribution with `power_df` degrees of freedom when calculating power. Defaults to `NULL`, in which case a Gaussian distribution is used;
#' * `na.rm`, whether to remove point estimates or standard errors where either (or both) is missing. Defaults to `TRUE`;
#' * `char.sep`, a character utilised when splitting the input dataset `data`. Generally, this should not be changed;
#' * `dropbig.max`, specifies the maximum acceptable absolute value of the point estimates, after standardisation. Defaults to 10;
#' * `dropbig.semax`, specifies the maximum acceptable absolute value of the standard error, after standardisation. Defaults to 100
#' * `dropbig.robust`, specifies whether to use robust standardisation (using median and inter-quartile range) rather than normal standardisation (using mean and standard deviation). Defaults to `TRUE`, in which case robust standardisation will be used for `dropbig`.
#' @return An object of class `simsum`.
#' @references White, I.R. 2010. simsum: Analyses of simulation studies including Monte Carlo error. The Stata Journal 10(3): 369-385. \url{https://www.stata-journal.com/article.html?article=st0200}
#' @references Morris, T.P., White, I.R. and Crowther, M.J. 2019. _Using simulation studies to evaluate statistical methods_. Statistics in Medicine, \doi{10.1002/sim.8086}
#' @references Gasparini, A. 2018. rsimsum: Summarise results from Monte Carlo simulation studies. Journal of Open Source Software 3(26):739, \doi{10.21105/joss.00739}
#' @export
#' @details
#' The following names are not allowed for any column in `data` that is passed to [simsum()]: `stat`, `est`, `mcse`, `lower`, `upper`, `:methodvar`, `:true`.
#'
#' @examples
#' data("MIsim", package = "rsimsum")
#' s <- simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method", ref = "CC")
#' # If 'ref' is not specified, the reference method is inferred
#' s <- simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method")
simsum <- function(data,
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
                   control = list()) {
  ### Check arguments
  arg_checks <- checkmate::makeAssertCollection()
  # 'data' must be a data.frame
  checkmate::assert_data_frame(x = data, add = arg_checks)
  # 'estvarname', 'se', 'methodvar', 'ref' must be a single string value
  checkmate::assert_string(x = estvarname, add = arg_checks)
  checkmate::assert_string(x = se, null.ok = TRUE, add = arg_checks)
  checkmate::assert_string(x = ref, null.ok = TRUE, add = arg_checks)
  checkmate::assert_string(x = df, null.ok = TRUE, add = arg_checks)
  # 'true' must be a single numeric value, or a string that identifies a column in 'data'
  if (!is.null(true)) {
    checkmate::assert_true(x = inherits(x = true, what = c("character", "numeric", "integer")), add = arg_checks)
    if (is.character(true)) {
      checkmate::assert_string(x = true, add = arg_checks)
      checkmate::assert_true(x = all(true %in% names(data)), add = arg_checks)
    } else {
      checkmate::assert_number(x = true, null.ok = TRUE, add = arg_checks)
    }
  }
  # 'dropbig', 'mcse', 'x' must be single logical value
  checkmate::assert_logical(x = dropbig, len = 1, add = arg_checks)
  checkmate::assert_logical(x = x, len = 1, add = arg_checks)
  # 'by' and 'methodvar' must be a vector of strings; can be NULL
  checkmate::assert_character(x = methodvar, null.ok = TRUE, add = arg_checks)
  checkmate::assert_character(x = by, null.ok = TRUE, add = arg_checks)
  # 'estvarname', 'se' must be in 'data'; all elements of 'by' must be in 'data'; 'methodvar' must be in 'data'; 'df' must be in 'data'
  checkmate::assert_subset(x = estvarname, choices = names(data), add = arg_checks)
  checkmate::assert_subset(x = se, choices = names(data), add = arg_checks)
  checkmate::assert_subset(x = by, choices = names(data), add = arg_checks)
  checkmate::assert_subset(x = methodvar, choices = names(data), add = arg_checks)
  checkmate::assert_subset(x = df, choices = names(data), add = arg_checks)
  # 'estvarname', 'se', 'methodvar', 'by' , 'df' must not be any in ('stat', 'est', 'mcse', 'lower', 'upper', ':methodvar', ':true')
  .private_names <- c("stat", "est", "mcse", "lower", "upper", ":methodvar", ":true")
  .check_private(var = estvarname, label = "estvarname", private_names = .private_names)
  .check_private(var = se, label = "se", private_names = .private_names)
  .check_private(var = methodvar, label = "methodvar", private_names = .private_names)
  .check_private(var = by, label = "by", private_names = .private_names)
  .check_private(var = df, label = "df", private_names = .private_names)
  # Process vector of 'methodvar' if a vector
  user_methodvar <- NULL
  if (length(methodvar) > 1) {
    reftable <- .compact_method_columns(data = data, methodvar = methodvar)$reftable
    data <- .compact_method_columns(data = data, methodvar = methodvar)$data
    user_methodvar <- methodvar
    methodvar <- ":methodvar"
  }
  # 'ref' must be one of the options in 'methodvar'
  if (!is.null(methodvar)) {
    checkmate::assert_subset(x = ref, choices = as.character(unique(data[[methodvar]])), add = arg_checks)
  }
  # 'ci.limits' must be either a numeric vector of length 2 or a string vector with column names in 'data'
  if (!is.null(ci.limits)) {
    checkmate::assert_true(x = inherits(x = ci.limits, what = c("character", "numeric", "integer")), add = arg_checks)
    if (is.character(ci.limits)) {
      checkmate::assert_character(x = ci.limits, len = 2, add = arg_checks)
      checkmate::assert_true(x = all(ci.limits %in% names(data)), add = arg_checks)
      lapply(X = ci.limits, FUN = function(x) .check_private(var = x, label = "ci.limits", private_names = .private_names))
    }
    if (is.numeric(ci.limits)) {
      checkmate::assert_numeric(x = ci.limits, len = 2, add = arg_checks)
    }
  }
  # 'control' must be a list, with well defined components
  checkmate::assert_list(x = control, add = arg_checks)
  checkmate::assert_subset(x = names(control), choices = c("mcse", "level", "power_df", "na.rm", "char.sep", "dropbig.max", "dropbig.semax", "dropbig.robust"), empty.ok = TRUE, add = arg_checks)
  checkmate::assert_logical(x = control$mcse, len = 1, null.ok = TRUE, add = arg_checks)
  checkmate::assert_number(x = control$level, lower = 0, upper = 1, null.ok = TRUE, add = arg_checks)
  checkmate::assert_number(x = control$power_df, null.ok = TRUE, add = arg_checks)
  checkmate::assert_logical(x = control$na.rm, len = 1, null.ok = TRUE, add = arg_checks)
  checkmate::assert_string(x = control$char.sep, null.ok = TRUE, add = arg_checks)
  checkmate::assert_number(x = control$dropbig.max, null.ok = TRUE, add = arg_checks)
  checkmate::assert_number(x = control$dropbig.semax, null.ok = TRUE, add = arg_checks)
  checkmate::assert_logical(x = control$dropbig.robust, len = 1, null.ok = TRUE, add = arg_checks)
  # Report
  if (!arg_checks$isEmpty()) checkmate::reportAssertions(arg_checks)

  ### Only one of 'ci.limits' and 'df' can be specified
  if (!is.null(ci.limits) & !is.null(df)) stop("Only one of 'ci.limits' and 'df' can be specified.", call. = FALSE)

  ### Set control parameters
  control.default <- list(mcse = TRUE, level = 0.95, power_df = NULL, na.rm = TRUE, char.sep = "~", dropbig.max = 10, dropbig.semax = 100, dropbig.robust = TRUE)
  control.tmp <- unlist(list(
    control[names(control) %in% names(control.default)],
    control.default[!(names(control.default) %in% names(control))]
  ), recursive = FALSE)
  control <- control.tmp

  ### Add hidden column with true values
  if (!is.null(true)) {
    if (is.character(true)) {
      data[[":true"]] <- data[[true]]
    } else {
      data[[":true"]] <- true
    }
  }

  ### Factorise 'methodvar', 'by'
  data <- .factorise(data = data, cols = c(methodvar, by))

  ### Check that levels of factors are ok
  .validate_levels(data = data, cols = c(methodvar, by), char = control$char.sep)

  ### Set reference method if `ref` is not specified
  if (!is.null(methodvar)) {
    methods <- levels(data[[methodvar]])
    if (is.null(ref)) {
      ref <- methods[1]
      message(paste("'ref' method was not specified,", ref, "set as the reference"))
    }
    data[[methodvar]] <- relevel(data[[methodvar]], ref = ref)
  }

  ### Throw a warning if `ref` is specified and `methodvar` is not
  if (is.null(methodvar) & !is.null(ref)) {
    warning("'ref' method is specified while 'methodvar' is not: 'ref' will be ignored")
    ref <- NULL
  }

  ### Identify and drop (if required) point estimates and standard errors that are too big
  if (dropbig) {
    data <- .dropbig(data = data, estvarname = estvarname, se = se, methodvar = methodvar, by = by, max = control$dropbig.max, semax = control$dropbig.semax, robust = control$dropbig.robust)
  }

  ### Drop estimates if SE is missing, and vice versa
  data <- .na_pair(data = data, estvarname = estvarname, se = se)

  ### Compute summary statistics
  # Split by first
  data <- .split_by(data = data, by = by)

  # Then, split methodvar
  data <- lapply(X = seq_along(data), FUN = function(i) .split_by(data = data[[i]], by = methodvar))

  # Remove elements in 'data' where we get empty datasets (#47)
  data <- .drop_empty_splits(data)

  # Then call .performance to compute all performance measures
  summ <- lapply(X = seq_along(data), FUN = function(i) {
    if (!is.null(methodvar)) {
      rho <- vapply(X = methods, FUN = function(x) stats::cor(data[[i]][[ref]][[estvarname]], data[[i]][[x]][[estvarname]], use = ifelse(control$na.rm, "na.or.complete", "everything")), FUN.VALUE = numeric(1))
    } else {
      rho <- NULL
    }
    out.out <- lapply(X = seq_along(data[[i]]), FUN = function(j) {
      if (!is.null(methodvar)) {
        empse_ref <- sqrt(stats::var(data[[i]][[ref]][[estvarname]], na.rm = control$na.rm))
      } else {
        empse_ref <- NULL
      }
      out.in <- .performance(data = data[[i]][[j]], estvarname = estvarname, se = se, true = true, rho = rho[names(data[[i]][j])], empse_ref = empse_ref, ci.limits = ci.limits, df = df, control = control)
      if (!is.null(methodvar)) {
        out.in[[methodvar]] <- unique(data[[i]][[j]][[methodvar]])
      }
      for (byval in by) {
        out.in[[byval]] <- unique(data[[i]][[j]][[byval]])
      }
      out.in
    })
    out.out <- .br(out.out)
    out.out
  })
  summ <- .br(summ)

  # If vector of 'methodvar', restore input data
  if (!is.null(user_methodvar)) {
    summ[[".nr"]] <- seq(nrow(summ))
    summ <- merge(summ, reftable, by = methodvar)
    summ <- summ[order(summ[[".nr"]]), ]
    summ[, methodvar] <- NULL
    summ[, ".nr"] <- NULL
    methodvar <- user_methodvar
  }

  ### Include stuff into object to return
  obj <- list()
  obj$summ <- summ
  obj$estvarname <- estvarname
  obj$true <- true
  obj$se <- se
  obj$methodvar <- methodvar
  obj$ref <- ref
  obj$dropbig <- dropbig
  obj$ci.limits <- ci.limits
  obj$df <- df
  obj$by <- by
  obj$control <- control
  if (x) {
    obj$x <- .br(lapply(data, .br))
    if (!is.null(true)) obj$x[[":true"]] <- NULL
    rownames(obj$x) <- NULL
  }

  ### Return object of class simsum
  class(obj) <- c("simsum", "list")
  return(obj)
}
