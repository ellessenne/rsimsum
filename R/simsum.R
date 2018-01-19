#' simsum
#'
#' @title Analyses of simulation studies including Monte Carlo error
#' @description `simsum` computes performance measures for simulation studies in which each simulated data set yields point estimates by one or more analysis methods. Bias, empirical standard error and precision relative to a reference method can be computed for each method.  If, in addition, model-based standard errors are available then `simsum` can compute the average model-based standard error, the relative error in the model-based standard error, the coverage of nominal confidence intervals, and the power to reject a null hypothesis. Monte Carlo errors are available for all estimated quantities.
#' @param data A `data.frame` in which variable names are interpreted. It has to be in tidy format, e.g. each variable forms a column and each observation forms a row.
#' @param estvarname The name of the variable containing the point estimates.
#' @param true The true value of the parameter. This is used in calculations of bias and coverage.
#' @param se The name of the variable containing the standard errors of the point estimates.
#' @param methodvar The name of the variable containing the methods to compare. Can be `NULL`.
#' @param ref Specifies the reference method against which relative precisions will be calculated. Only useful if `methodvar` is specified.
#' @param df If specified, a t distribution with `df` degrees of freedom is used when calculating coverage and power.
#' @param dropbig Specifies that point estimates or standard errors beyond the maximum acceptable values should be dropped.
#' @param max Specifies the maximum acceptable absolute value of the point estimates, standardised to mean 0 and SD 1. Defaults to `10`.
#' @param semax Specifies the maximum acceptable value of the standard error, as a multiple of the mean standard error. Defaults to `100`.
#' @param level Specifies the confidence level for coverage and power. Defaults to `0.95`.
#' @param by A vector of variable names to compute performance measures by a list of factors. Can be `NULL`.
#' @param mcse Reports Monte Carlo standard errors for all performance measures. Defaults to `TRUE`.
#' @param sanitise Sanitise column names passed to `simsum` by removing all dot characters (`.`), which could cause problems. Defaults to `TRUE`.
#' @param na.rm A logical value indicating whether missing values (`NA`) should be removed before the computation proceeds. Defaults to `TRUE`.
#' @return An object of class `simsum`.
#' @references White, I.R. 2010. simsum: Analyses of simulation studies including Monte Carlo error. The Stata Journal 10(3): 369-385
#' @references Morris, T.P, White, I.R. and Crowther, M.J. 2017. Using simulation studies to evaluate statistical methods. <arXiv:1712.03198>
#' @export
#'
#' @examples
#' data("MIsim")
#' s <- simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method", ref = "CC")
#' # If `ref` is not specified, the reference method is inferred
#' s <- simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method")

simsum <-
  function(data,
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
           mcse = TRUE,
           sanitise = TRUE,
           na.rm = TRUE) {
    ### Check arguments
    arg_checks <- checkmate::makeAssertCollection()

    # `data` must be a data.frame
    checkmate::assert_data_frame(data, add = arg_checks)

    # `estvarname`, `se`, `methodvar`, `ref` must be a single string value
    checkmate::assert_string(estvarname, add = arg_checks)
    checkmate::assert_string(se, add = arg_checks)
    checkmate::assert_string(methodvar, null.ok = TRUE, add = arg_checks)
    checkmate::assert_string(ref, null.ok = TRUE, add = arg_checks)

    # `true`, `df`, `max`, `semax`, `level` must be a single numberic value
    # `df` can be NULL
    checkmate::assert_number(true, add = arg_checks)
    checkmate::assert_number(df, null.ok = TRUE, add = arg_checks)
    checkmate::assert_number(max, add = arg_checks)
    checkmate::assert_number(semax, add = arg_checks)
    checkmate::assert_number(
      level,
      lower = 0,
      upper = 1,
      add = arg_checks
    )

    # `dropbig`, `mcse`, `sanitise`, `na.rm` must be single logical value
    checkmate::assert_logical(dropbig, len = 1, add = arg_checks)
    checkmate::assert_logical(mcse, len = 1, add = arg_checks)
    checkmate::assert_logical(sanitise, len = 1, add = arg_checks)
    checkmate::assert_logical(na.rm, len = 1, add = arg_checks)

    # `by` must be a vector of strings; can be NULL
    checkmate::assert_character(by, null.ok = TRUE, add = arg_checks)

    # `estvarname`, `se` must be in `data`; all elements of `by` must be in data; `methodvar` must be in data
    checkmate::assert_subset(estvarname, choices = names(data), add = arg_checks)
    checkmate::assert_subset(se, choices = names(data), add = arg_checks)
    checkmate::assert_subset(by, choices = names(data), add = arg_checks)
    checkmate::assert_subset(methodvar, choices = names(data), add = arg_checks)

    # `ref` must be one of the options in `methodvar`
    if (!is.null(methodvar)) {
      checkmate::assert_subset(ref, choices = as.character(unique(data[[methodvar]])), add = arg_checks)
    }

    ### Report if there are any errors
    if (!arg_checks$isEmpty()) {
      checkmate::reportAssertions(arg_checks)
    }

    ### Set reference method if `ref` is not specified
    if (!is.null(methodvar)) {
      methods <- sort(unique(data[[methodvar]]))
      if (is.null(ref)) {
        message(paste("`ref` was not specified,", methods[1], "set as the reference"))
        ref <- methods[1]
      }
    }

    ### Throw a warning if `ref` is specified and `methodvar` is not
    if (is.null(methodvar) & !is.null(ref)) {
      warning("`ref` is specified while `methodvar` is not; `ref` will be ignored")
      ref <- NULL
    }

    ### Sanitise names if required
    if (sanitise) {
      names(data) <- gsub(
        pattern = ".",
        replacement = "",
        x = names(data),
        fixed = TRUE
      )
      if (!is.null(estvarname)) {
        estvarname <- gsub(
          pattern = ".",
          replacement = "",
          x = estvarname,
          fixed = TRUE
        )
      }
      if (!is.null(se)) {
        se <- gsub(
          pattern = ".",
          replacement = "",
          x = se,
          fixed = TRUE
        )
      }
      if (!is.null(methodvar)) {
        methodvar <- gsub(
          pattern = ".",
          replacement = "",
          x = methodvar,
          fixed = TRUE
        )
      }
      if (!is.null(by)) {
        by <- gsub(
          pattern = ".",
          replacement = "",
          x = by,
          fixed = TRUE
        )
      }
    }

    ### Identify and drop (if required) point estimates and standard errors that are too big
    if (dropbig) {
      # Save observations which one are too big
      big_estvarname <- data.frame(
        rownumber = which((data[[estvarname]] - mean(data[[estvarname]])) / sqrt(stats::var(data[[estvarname]])) >= max),
        value = data[[estvarname]][(data[[estvarname]] - mean(data[[estvarname]])) / sqrt(stats::var(data[[estvarname]])) >= max]
      )
      big_se <- data.frame(
        rownumber = which(data[[se]] >= mean(data[[se]]) * semax),
        value = data[[se]][data[[se]] >= mean(data[[se]]) * semax]
      )
      # Drop 'em
      data[[estvarname]][(data[[estvarname]] - mean(data[[estvarname]])) / sqrt(stats::var(data[[estvarname]])) >= max] <- NA
      data[[se]][data[[se]] >= mean(data[[se]]) * semax] <- NA
    }

    ### Compute summary statistics
    if (is.null(by)) {
      # No `by` factors
      if (is.null(methodvar)) {
        # No `methodvar`, no `by`
        summ <- perfms(
          data = data,
          estvarname = estvarname,
          true = true,
          se = se,
          ref = ref,
          level = level,
          df = df,
          mcse = mcse,
          na.rm = na.rm
        )
      } else {
        # With `methodvar`, no `by`
        # Split data
        methodvar_split <- split(x = data, f = lapply(methodvar, function(f)
          data[[f]]))

        # Compute correlations
        rho <- vapply(
          X = methods,
          FUN = function(x)
            stats::cor(methodvar_split[[ref]][[estvarname]], methodvar_split[[x]][[estvarname]]),
          FUN.VALUE = numeric(1)
        )
        # Compute n. of observations used in computing correlations
        summ <- lapply(
          X = methods,
          FUN = function(x)
            perfms(
              data = methodvar_split[[x]],
              estvarname = estvarname,
              true = true,
              se = se,
              ref = ref,
              method = x,
              methodvar = methodvar,
              level = level,
              df = df,
              mcse = mcse,
              na.rm = na.rm,
              empse_ref = sqrt(stats::var(methodvar_split[[ref]][[estvarname]])),
              rho = rho[x]
            )
        )
        summ <- do.call(rbind.data.frame, summ)
      }
    } else {
      # Split data by `by` factors
      by_split <- split(
        data,
        f = lapply(by, function(f)
          data[[f]])
      )

      summ <- lapply(seq_along(by_split), function(i) {
        # No `methodvar`
        if (is.null(methodvar)) {
          summ <- perfms(
            data = by_split[[i]],
            estvarname = estvarname,
            true = true,
            se = se,
            ref = ref,
            level = level,
            df = df,
            mcse = mcse,
            na.rm = na.rm,
            by = by,
            byvalues = names(by_split)[i]
          )
          return(summ)
        } else {
          # With `methodvar`
          methodvar_split <- split(
            by_split[[i]],
            f = lapply(methodvar, function(f)
              by_split[[i]][[f]])
          )
          rho <- vapply(
            X = methods,
            FUN = function(x)
              stats::cor(methodvar_split[[ref]][[estvarname]], methodvar_split[[x]][[estvarname]]),
            FUN.VALUE = numeric(1)
          )
          summ <- lapply(
            X = methods,
            FUN = function(x)
              perfms(
                data = methodvar_split[[x]],
                estvarname = estvarname,
                true = true,
                se = se,
                ref = ref,
                method = x,
                methodvar = methodvar,
                level = level,
                df = df,
                mcse = mcse,
                na.rm = na.rm,
                empse_ref = sqrt(stats::var(methodvar_split[[ref]][[estvarname]])),
                rho = rho[x],
                by = by,
                byvalues = names(by_split)[i]
              )
          )
          summ <- do.call(rbind.data.frame, summ)
          return(summ)
        }
      })
      summ <- do.call(rbind.data.frame, summ)
    }

    ### Include call and other info into object to return
    obj <- list()
    obj$call <- match.call()
    obj$summ <- summ
    obj$estvarname <- estvarname
    obj$true <- true
    obj$se <- se
    obj$methodvar <- methodvar
    obj$ref <- ref
    obj$df <- df
    obj$dropbig <- dropbig
    if (dropbig) {
      obj$big_estvarname <- big_estvarname
      obj$big_se <- big_se
    }
    obj$max <- max
    obj$semax <- semax
    obj$level <- level
    obj$by <- by
    obj$mcse <- mcse
    obj$sanitise <- sanitise
    obj$na.rm <- na.rm

    ### Return object of class simsum
    class(obj) <- c("list", "simsum")
    return(obj)
  }

perfms <-
  function(data,
           estvarname,
           true,
           se,
           ref,
           method = NULL,
           methodvar = NULL,
           level,
           df,
           mcse,
           empse_ref = NULL,
           rho = NULL,
           by = NULL,
           byvalues = NULL,
           na.rm) {
    ### Make object to return
    obj <- list()

    ### Compute performance measures
    # Number of non-missing point estimates, standard errors:
    nsim <- sum(!is.na(data[[estvarname]]) &
      !is.na(data[[se]]), na.rm = na.rm)
    # Mean, median and variance of betas
    theta_mean <- mean(data[[estvarname]], na.rm = na.rm)
    theta_median <- stats::median(data[[estvarname]], na.rm = na.rm)
    theta_var <- stats::var(data[[estvarname]], na.rm = na.rm)
    # Mean, median and average of ses
    se2_mean <- mean(data[[se]] ^ 2, na.rm = na.rm)
    se2_median <- stats::median(data[[se]] ^ 2, na.rm = na.rm)
    se2_var <- stats::var(data[[se]] ^ 2, na.rm = na.rm)
    # Bias
    bias <- 1 / nsim * sum(data[[estvarname]] - true, na.rm = na.rm)
    # Empirical standard error
    empse <- sqrt(1 / (nsim - 1) * sum((data[[estvarname]] - mean(data[[estvarname]])) ^ 2, na.rm = na.rm))
    # Mean squared error
    mse <- 1 / nsim * sum((data[[estvarname]] - true) ^ 2, na.rm = na.rm)
    # Relative change in precision
    if (!is.null(empse_ref) & !is.null(rho)) {
      relprec <- (empse_ref / empse) ^ 2
    } else {
      relprec <- NA
    }
    names(relprec) <- NULL
    # Model-based standard error
    modelse <- sqrt(1 / nsim * sum(data[[se]] ^ 2, na.rm = na.rm))
    # Relative error in model-based standard error
    relerror <- 100 * (modelse / empse - 1)
    # Compute critical value from either a normal or a t distribution
    crit <- ifelse(is.null(df), stats::qnorm(1 - (1 - level) / 2), stats::qt(1 - (1 - level) / 2, df = df))
    # Coverage of a nominal (1 - level)% confidence interval
    cover <- 1 / nsim * sum(true >= data[[estvarname]] - crit * data[[se]] & true <= data[[estvarname]] + crit * data[[se]], na.rm = na.rm)
    # Bias-corrected coverage of a nominal (1 - level)% confidence interval
    bccover <- 1 / nsim * sum(mean(data[[estvarname]]) >= data[[estvarname]] - crit * data[[se]] & mean(data[[estvarname]]) <= data[[estvarname]] + crit * data[[se]], na.rm = na.rm)
    # Power of a significance test at the `level` level
    power <- 1 / nsim * sum(abs(data[[estvarname]]) >= crit * data[[se]], na.rm = na.rm)

    ### Compute Monte Carlo SEs if requested:
    if (mcse) {
      bias_mcse <- sqrt(1 / (nsim * (nsim - 1)) * sum((data[[estvarname]] - mean(data[[estvarname]])) ^ 2, na.rm = na.rm))
      empse_mcse <- empse / sqrt(2 * (nsim - 1))
      mse_mcse <- sqrt(sum(((data[[estvarname]] - true) ^ 2 - mse) ^ 2, na.rm = na.rm) / (nsim * (nsim - 1)))
      if (!is.null(empse_ref) & !is.null(rho)) {
        relprec_mcse <- 2 * (empse_ref) ^ 2 / (empse) ^ 2 * sqrt((1 - rho ^ 2) / (nsim - 1))
      } else {
        relprec_mcse <- NA
      }
      names(relprec_mcse) <- NULL
      modelse_mcse <- sqrt(se2_var / (4 * nsim * modelse ^ 2))
      relerror_mcse <- 100 * (modelse / empse) * sqrt(se2_var / (4 * nsim * modelse ^ 4) + 1 / (2 * nsim - 1))
      cover_mcse <- sqrt(cover * (1 - cover) / nsim)
      bccover_mcse <- sqrt(bccover * (1 - bccover) / nsim)
      power_mcse <- sqrt(power * (1 - power) / nsim)
    }

    ### Assemble object to return
    obj$stat <- c(
      "nsim",
      "thetamean",
      "thetamedian",
      "se2mean",
      "se2median",
      "bias",
      "empse",
      "mse",
      "relprec",
      "modelse",
      "relerror",
      "cover",
      "bccover",
      "power"
    )
    obj$coef <- c(
      nsim,
      theta_mean,
      theta_median,
      se2_mean,
      se2_median,
      bias,
      empse,
      mse,
      relprec,
      modelse,
      relerror,
      cover,
      bccover,
      power
    )
    if (mcse) {
      obj$mcse <- c(
        rep(NA, 5),
        bias_mcse,
        empse_mcse,
        mse_mcse,
        relprec_mcse,
        modelse_mcse,
        relerror_mcse,
        cover_mcse,
        bccover_mcse,
        power_mcse
      )
    }
    if (!is.null(method)) {
      obj[[methodvar]] <- method
    }
    if (!is.null(by)) {
      byvalues <- unlist(strsplit(byvalues, ".", fixed = TRUE))
      for (w in seq_along(by)) {
        obj[[by[w]]] <- byvalues[w]
      }
    }
    obj <- as.data.frame(obj, stringsAsFactors = FALSE)

    ### Return object
    return(obj)
  }
