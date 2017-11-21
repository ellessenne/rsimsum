#' simsum
#'
#' @title Analyses of simulation studies including Monte Carlo error
#'
#' @description `simsum` computes performance measures for simulation studies in which each simulated data set yields point estimates by one or more analysis methods. Bias, empirical standard error and precision relative to a reference method can be computed for each method.  If, in addition, model-based standard errors are available then `simsum` can compute the average model-based standard error, the relative error in the model-based standard error, the coverage of nominal confidence intervals, and the power to reject a null hypothesis. Monte Carlo errors are available for all estimated quantities.
#'
#' @param data A `data.frame` in which variable names are interpreted. It has to be in tidy format, e.g. each variable forms a column and each observation forms a row.
#' @param estvarname The name of the variable containing the point estimates.
#' @param true The true value of the parameter. This is used in calculations of bias and coverage.
#' @param methodvar The name of the variable containing the methods to compare. Can be `NULL`.
#' @param se The name of the variable containing the standard errors of the point estimates.
#' @param max Specifies the maximum acceptable absolute value of the point estimates, standardised to mean 0 and SD 1. Defaults to `10`.
#' @param semax Specifies the maximum acceptable value of the standard error, as a multiple of the mean standard error. Defaults to `100`.
#' @param dropbig Specifies that point estimates or standard errors beyond the maximum acceptable values should be dropped.
#' @param level Specifies the confidence level for coverage and power. Defaults to `0.95`.
#' @param by A vector of variable names to compute performance measures by a list of factors. Can be `NULL`.
#' @param mcse Reports Monte Carlo standard errors for all performance measures. Defaults to `TRUE`.
#' @param robust Specifies that robust Monte Carlo standard errors for the performance measures empirical standard error, relative gain in precision, relative error should be returned. More details in White (2010). Only useful when `mcse = TRUE`. Not yet implemented.
#' @param modelsemethod Specifies whether the model standard error should be computed as the root mean squared value (`modelsemethod = rmse`) or as the arithmetic mean (`modelsemethod = mean`). Defaults to `rmse`.
#' @param ref Specifies the reference method against which relative precisions will be calculated. Only useful if `methodvar` is specified.
#' @param sanitise Sanitise column names passed to `simsum` by removing all dot characters (`.`). Defaults to `TRUE`.
#'
#' @return An object of class `simsum`.
#'
#' @references White, I.R. 2010. simsum: Analyses of simulation studies including Monte Carlo error. The Stata Journal 10(3): 369-385
#'
#' @export
#'
#' @examples
#'
#' data("MIsim")
#'
#' s <- simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method", ref = "CC")
#'
#' # If `ref` is not specified, the reference method is inferred
#' s <- simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method")

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
			summ = perfms(data = data,
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
			summ = lapply(X = methods,
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
			summ = do.call(rbind.data.frame, summ)
		}
	} else {
		# Split data by `by` factors
		by_split = split(data,
										 f = lapply(by, function(f) data[[f]]))

		summ = lapply(seq_along(by_split), function(i) {
			# No `methodvar`
			if (is.null(methodvar)) {
				summ = perfms(data = by_split[[i]],
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
				return(summ)
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
				summ = lapply(X = methods,
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
				summ = do.call(rbind.data.frame, summ)
				return(summ)
			}
		})
		summ = do.call(rbind.data.frame, summ)
	}

	### Include call and other info into object to return
	obj = list()
	obj$call = match.call()
	obj$summ = summ
	obj$estvarname = estvarname
	obj$true = true
	obj$se = se
	obj$methodvar = methodvar
	obj$ref = ref
	obj$df = df
	obj$dropbig = dropbig
	obj$max = max
	obj$semax = semax
	obj$level = level
	obj$by = by
	obj$sanitise = sanitise
	obj$mcse = mcse
	obj$robust = robust
	obj$modelsemethod = modelsemethod

	### Return object of class simsum
	obj = structure(obj, class = c("simsum", "list"))
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
	obj$stat = c("bsims", "sesims", "bias", "esd", "mse", "relprec", "modelse", "relerror", "cover", "power")
	obj$coef = c(bsims, sesims, bias, esd, mse, relprec, modelse, relerror, cover, power)
	obj$mcse = c(NA, NA, bias_mcse, esd_mcse, mse_mcse, relprec_mcse, modelse_mcse, relerror_mcse, cover_mcse, power_mcse)
	if (!is.null(method)) obj$method = method
	if (!is.null(by)) {
		byvalues = unlist(strsplit(byvalues, ".", fixed = TRUE))
		for (w in seq_along(by)) {
			obj[[by[w]]] = byvalues[w]
		}
	}
	obj = as.data.frame(obj, stringsAsFactors = FALSE)
	return(obj)
}
