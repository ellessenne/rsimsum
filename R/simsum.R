#' simsum
#'
#' @description
#' @param data A `data.frame` in which variable names are interpreted.
#' @param estvarname a variable containing the point estimates
#' @param true gives the true value of the parameter.  This is used in calculations of bias and coverage and is required whenever these performance measures are requested.
#' @param methodvar identifies the method
#' @param se lists the names of the variables containing the standard errors of the point estimates.  For data in long format, this is a single variable.
#' @param max specifies the maximum acceptable absolute value of the point estimates, standardised to mean 0 and SD 1. The default value is 10.
#' @param semax specifies the maximum acceptable value of the standard error, as a multiple of the mean standard error.  The default value is 100.
#' @param dropbig specifies that point estimates or standard errors beyond the maximum acceptable values should be dropped.
#' @param level specifies the confidence level for coverages and powers. Default is 0.95.
#' @param by  computes performance measures by varlist. Multiple factors are collapsed into a single one using [base::interaction()]
#' @param mcse reports Monte Carlo standard errors for all performance measures.
#' @param robust  is only useful if mcse is also specified.  It requests robust Monte Carlo standard errors for the performance measures `empse`, `relprec` and `relerror`, instead of those based on an assumption of normally distributed point estimates.
#' @param modelsemethod specifies whether the model standard error should be computed as the root mean squared value (the default) or as the arithmetic mean.
#' @param ref specifies the reference method against which relative precisions will be calculated.
#' @param perfmeasures If none of the following options is specified, then all available performance measures are computed. Possible values are:
#' * `bsims` reports the number of simulations with non-missing point estimates.
#' * `sesims` reports the number of simulations with non-missing standard errors.
#' * `bias` estimates the bias in the point estimates.
#' * `empse` estimates the empirical standard error -- the standard deviation of the point estimates.
#' * `relprec` estimates the relative precision -- the inverse squared ratio of the empirical standard error of this method to the empirical standard error of the reference method.  This calculation is slow: 	omitting it can reduce run time by up to 90%.
#' * `mse` estimates the mean squared error.
#' * `modelse` estimates the model-based standard error. See modelsemethod() above.
#' * `relerror` estimates the proportional error in the model-based standard error, using the empirical standard error as gold standard.
#' * `cover` estimates the coverage of nominal confidence intervals at the specified level.
#' * `power` estimates the power to reject the null hypothesis that the true parameter is zero, at the specified level.
#' @param sanitise Sanitise column names passed to `simsum` by removing all dot characters (`.`).
#'
#' @return An object of class `simsum`.
#'
#' @details Beware of `by` or `methodvar` whose levels contain dots (`.`)!
#' @export
#'
#' @examples
#' \dontrun{
#' x = simsum(...)
#' }

simsum <- function(data,
									 estvarname,
									 true,
									 se,
									 methodvar = NULL,
									 ref = NULL,
									 df = NULL,
									 dropbig = FALSE,
									 max = 10,
									 semax = 100,
									 level = 0.95,
									 by = NULL,
									 sanitise = TRUE,
									 mcse = FALSE,
									 robust = FALSE,
									 modelsemethod = "rmse") {
	### Check arguments
	arg_checks = makeAssertCollection()

	# `data` must be a data.frame
	assert_data_frame(data, add = arg_checks)

	# `estvarname`, `se`, `methodvar`, `ref` must be a single string value
	assert_string(estvarname, add = arg_checks)
	assert_string(se, add = arg_checks)
	assert_string(methodvar, null.ok = TRUE, add = arg_checks)
	assert_string(ref, null.ok = TRUE, add = arg_checks)

	# `true`, `df`, `max`, `semax`, `level` must be a single numberic value
	# `df` can be NULL
	assert_number(true, add = arg_checks)
	assert_number(df, null.ok = TRUE, add = arg_checks)
	assert_number(max, add = arg_checks)
	assert_number(semax, add = arg_checks)
	assert_number(level, add = arg_checks)

	# `dropbig`, `mcse`, `robust`, `sanitise` must be single logical value
	assert_logical(dropbig, len = 1, add = arg_checks)
	assert_logical(mcse, len = 1, add = arg_checks)
	assert_logical(robust, len = 1, add = arg_checks)
	assert_logical(sanitise, len = 1, add = arg_checks)

	# `by` must be a vector of strings; can be NULL
	assert_character(by, null.ok = TRUE, add = arg_checks)

	# `modelsemethod` must be `rmse` or `mean`
	assert_choice(modelsemethod, choices = c("rmse", "mean"), add = arg_checks)

	### Report if there are any errors
	if (!arg_checks$isEmpty()) reportAssertions(arg_checks)

	### Check if `estvarname`, `se` in `data`
	errvec = character()
	for (args in c(estvarname, se)) {
		msg = validate_that(args %in% names(data), msg = args)
		if (!is.logical(msg)) errvec = c(errvec, msg)
	}
	if (length(errvec) > 0) stop(paste("The following variables are not in `data`:", paste(errvec, collapse = ", ")))

	### Check if all elements of `by` are in `data`
	if (!is.null(by)) {
		errvec = character()
		for (args in by) {
			msg = validate_that(args %in% names(data), msg = args)
			if (!is.logical(msg)) errvec = c(errvec, msg)
		}
		if (length(errvec) > 0) stop(paste("The following `by` variables are not in `data`:", paste(errvec, collapse = ", ")))
	}

	### Check that level is a value between 0 and 1
	if (level < 0 | level > 1) stop("`level` must be a value between 0 and 1")

	### Check that `methodvar` is in `data`, and that ref` value is in `methodvar`
	if (!is.null(methodvar)) {
		if (!(methodvar %in% names(data))) stop("`methodvar` not in `data`")
		# Make vector of unique methods
		methods = sort(unique(data[[methodvar]]))

		if (!is.null(ref)) {
			if (!(ref %in% methods)) stop(paste("The reference method", ref, "cannot be found in `methodvar`"))
		} else {
			# If ref is not specified, set the first method as the reference one and throw a warning
			message(paste("`ref` was not specified,", methods[1], "set as the reference"))
			ref = methods[1]
		}
	}

	### Throw a warning if `ref` is specified and `methodvar` is not
	if (is.null(methodvar) & !is.null(ref)) {
		warning("`ref` is specified while `methodvar` is not; `ref` will be ignored")
		ref = NULL
	}

	### Sanitise names
	if (sanitise) {
		names(data) = gsub(pattern = ".", replacement = "", x = names(data), fixed = TRUE)
		if (!is.null(estvarname)) estvarname = gsub(pattern = ".", replacement = "", x = estvarname, fixed = TRUE)
		if (!is.null(se)) se = gsub(pattern = ".", replacement = "", x = se, fixed = TRUE)
		if (!is.null(methodvar)) methodvar = gsub(pattern = ".", replacement = "", x = methodvar, fixed = TRUE)
		if (!is.null(by)) by = gsub(pattern = ".", replacement = "", x = by, fixed = TRUE)
	}

	### Compute summary statistics
	if (is.null(by)) {
		# No `by` factors
		if (is.null(methodvar)) {
			# No `methodvar`, no `by`
			obj = perfms(data = data,
									 estvarname = estvarname,
									 true = true,
									 se = se,
									 dropbig = dropbig,
									 max = max,
									 semax = semax,
									 ref = ref,
									 level = level,
									 df = df,
									 mcse = mcse,
									 robust = robust,
									 modelsemethod = modelsemethod)
		} else {
			# With `methodvar`, no `by`
			# Split data
			methodvar_split = split(x = data,
															f = lapply(methodvar, function(f) data[[f]]))

			# Compute correlations
			rho = vapply(X = methods,
									 FUN = function(x) cor(methodvar_split[[ref]][[estvarname]], methodvar_split[[x]][[estvarname]]),
									 FUN.VALUE = numeric(1))
			# Compute n. of observations used in computing correlations
			ncorr = vapply(X = methods,
										 FUN = function(x) sum(!is.na(methodvar_split[[ref]][[estvarname]]) & !is.na(methodvar_split[[x]][[estvarname]])),
										 FUN.VALUE = numeric(1))
			obj = lapply(X = methods,
									 FUN = function(x) perfms(data = methodvar_split[[x]],
									 												 estvarname = estvarname,
									 												 true = true,
									 												 se = se,
									 												 dropbig = dropbig,
									 												 max = max,
									 												 semax = semax,
									 												 ref = ref,
									 												 method = x,
									 												 level = level,
									 												 df = df,
									 												 mcse = mcse,
									 												 robust = robust,
									 												 modelsemethod = modelsemethod,
									 												 esd_ref = sqrt(var(methodvar_split[[ref]][[estvarname]])),
									 												 rho = rho[x],
									 												 ncorr = ncorr[x]))
			obj = do.call(rbind.data.frame, obj)
		}
	} else {
		# Split data by `by` factors
		by_split = split(data,
										 f = lapply(by, function(f) data[[f]]))

		obj = lapply(seq_along(by_split), function(i) {
			# No `methodvar`
			if (is.null(methodvar)) {
				obj = perfms(data = by_split[[i]],
										 estvarname = estvarname,
										 true = true,
										 se = se,
										 dropbig = dropbig,
										 max = max,
										 semax = semax,
										 ref = ref,
										 level = level,
										 df = df,
										 mcse = mcse,
										 robust = robust,
										 modelsemethod = modelsemethod,
										 by = by,
										 byvalues = names(by_split)[i])
				# obj$method = " "
				return(obj)
			} else {
				# With `methodvar`
				methodvar_split = split(by_split[[i]],
																f = lapply(methodvar, function(f) by_split[[i]][[f]]))
				rho = vapply(X = methods,
										 FUN = function(x) cor(methodvar_split[[ref]][[estvarname]], methodvar_split[[x]][[estvarname]]),
										 FUN.VALUE = numeric(1))
				ncorr = vapply(X = methods,
											 FUN = function(x) sum(!is.na(methodvar_split[[ref]][[estvarname]]) & !is.na(methodvar_split[[x]][[estvarname]])),
											 FUN.VALUE = numeric(1))
				obj = lapply(X = methods,
										 FUN = function(x) perfms(data = methodvar_split[[x]],
										 												 estvarname = estvarname,
										 												 true = true,
										 												 se = se,
										 												 dropbig = dropbig,
										 												 max = max,
										 												 semax = semax,
										 												 ref = ref,
										 												 method = x,
										 												 level = level,
										 												 df = df,
										 												 mcse = mcse,
										 												 robust = robust,
										 												 modelsemethod = modelsemethod,
										 												 esd_ref = sqrt(var(methodvar_split[[ref]][[estvarname]])),
										 												 rho = rho[x],
										 												 ncorr = ncorr[x],
										 												 by = by,
										 												 byvalues = names(by_split)[i]))
				obj = do.call(rbind.data.frame, obj)
				return(obj)
			}
		})
		obj = do.call(rbind.data.frame, obj)
	}

	# Return object of class simsum
	# obj = structure(obj, class = "simsum")
	return(obj)
}

perfms <- function(data,
									 estvarname,
									 true,
									 se,
									 dropbig,
									 max,
									 semax,
									 ref,
									 method = NULL,
									 level,
									 df,
									 mcse,
									 robust,
									 modelsemethod,
									 esd_ref = NULL,
									 rho = NULL,
									 ncorr = NULL,
									 by = NULL,
									 byvalues = NULL) {

	### Make object to return
	obj = list()

	### Check for too big estimates / standard errors
	# Save which one are too big as an attribute
	attr(obj, "big_estvarname") = data.frame(
		rownumber = which((data[[estvarname]] - mean(data[[estvarname]])) / sqrt(var(data[[estvarname]])) >= max),
		value = data[[estvarname]][(data[[estvarname]] - mean(data[[estvarname]])) / sqrt(var(data[[estvarname]])) >= max])
	attr(obj, "big_se") = data.frame(
		rownumber = which(data[[se]] >= mean(data[[se]]) * semax),
		value = data[[se]][data[[se]] >= mean(data[[se]]) * semax])
	# Drop them if required
	if (dropbig) {
		data[[estvarname]][(data[[estvarname]] - mean(data[[estvarname]])) / sqrt(var(data[[estvarname]])) >= max] = NA
		data[[se]][data[[se]] >= mean(data[[se]]) * semax] = NA
	}

	### Compute performance measures
	# Number of non-missing standard errors:
	bsims = sum(!is.na(data[[estvarname]]))
	sesims = sum(!is.na(data[[se]]))
	bothsims = sum(!is.na(data[[estvarname]]) & !is.na(data[[se]]))

	# Mean and variance of betas
	beta_mean = mean(data[[estvarname]])
	beta_var = var(data[[estvarname]])

	# Mean and average of ses
	se2_mean = mean(data[[se]] ^ 2)
	se2_var = var(data[[se]] ^ 2)

	# Bias
	bias = beta_mean - true
	bias_mcse = sqrt(beta_var / bsims)

	# Empirical standard deviation
	esd = sqrt(beta_var)
	esd_mcse = esd / sqrt(2 * (bsims - 1))

	# Mean squared error
	mse = mean((data[[estvarname]] - true) ^ 2)
	mse_mcse = sqrt(var((data[[estvarname]] - true) ^ 2)) / sqrt(bsims)

	# Relative change in precision
	if (!is.null(esd_ref) & !is.null(rho) & !is.null(ncorr)) {
		relprec = 100 * ((esd_ref / esd) ^ 2 - 1)
		relprec_mcse = 200 * (esd_ref / esd) ^ 2 * sqrt((1 - rho ^ 2) / (ncorr - 1))
	} else {
		relprec = NA
		relprec_mcse = NA
	}
	names(relprec) = NULL
	names(relprec_mcse) = NULL

	# Model-based standard error
	if (modelsemethod == "rmse") {
		modelse = sqrt(se2_mean)
		modelse_mcse = sqrt(se2_var / (4 * sesims * se2_mean))
	} else {
		modelse = mean(data[[se]])
		modelse_mcse = sqrt(var(data[[se]])) / sqrt(sesims)
	}

	# Relative error in model-based standard error
	relerror = 100 * (modelse / esd - 1)
	relerror_mcse = 100 * (modelse / esd) * sqrt((modelse_mcse / modelse) ^ 2 + (esd_mcse / esd) ^ 2)

	# Compute critical value from either a normal or a t distribution
	crit = ifelse(is.null(df), qnorm(1 - (1 - level) / 2), qt(1 - (1 - level) / 2, df = df))

	# Coverage of a nominal (1 - level)% confidence interval
	cover = mean(100 * (abs(data[[estvarname]] - true) < crit * data[[se]]))
	cover_mcse = sqrt(cover * (100 - cover) / bothsims)

	# Power of a significance test at the `level` level
	power = mean(100 * (abs(data[[estvarname]]) >= crit * data[[se]]))
	power_mcse = sqrt(power * (100 - power) / bothsims)

	### Assemble object to return
	obj$bsims = bsims
	obj$sesims = sesims
	obj$bias = bias
	obj$bias_mcse = bias_mcse
	obj$esd = esd
	obj$esd_mcse = esd_mcse
	obj$mse = mse
	obj$mse_mcse = mse_mcse
	obj$relprec = relprec
	obj$relprec_mcse = relprec_mcse
	obj$modelse = modelse
	obj$modelse_mcse = modelse_mcse
	obj$relerror = relerror
	obj$relerror_mcse = relerror_mcse
	obj$cover = cover
	obj$cover_mcse = cover_mcse
	obj$power = power
	obj$power_mcse = power_mcse
	if (!is.null(method)) obj$method = method
	if (!is.null(by)) {
		byvalues = unlist(strsplit(byvalues, ".", fixed = TRUE))
		for (w in seq_along(by)) {
			obj[[by[w]]] = byvalues[w]
		}
	}
	return(obj)
}
