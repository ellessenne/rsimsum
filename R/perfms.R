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
    se2_mean <- mean(data[[se]]^2, na.rm = na.rm)
    se2_median <- stats::median(data[[se]]^2, na.rm = na.rm)
    se2_var <- stats::var(data[[se]]^2, na.rm = na.rm)
    # Bias
    bias <- 1 / nsim * sum(data[[estvarname]] - true, na.rm = na.rm)
    # Empirical standard error
    empse <- sqrt(1 / (nsim - 1) * sum((data[[estvarname]] - mean(data[[estvarname]], na.rm = na.rm))^2, na.rm = na.rm))
    # Mean squared error
    mse <- 1 / nsim * sum((data[[estvarname]] - true)^2, na.rm = na.rm)
    # Relative change in precision
    if (!is.null(empse_ref) & !is.null(rho)) {
      relprec <- (empse_ref / empse)^2
    } else {
      relprec <- NA
    }
    names(relprec) <- NULL
    # Model-based standard error
    modelse <- sqrt(1 / nsim * sum(data[[se]]^2, na.rm = na.rm))
    # Relative error in model-based standard error
    relerror <- 100 * (modelse / empse - 1)
    # Compute critical value from either a normal or a t distribution
    crit <- ifelse(is.null(df), stats::qnorm(1 - (1 - level) / 2), stats::qt(1 - (1 - level) / 2, df = df))
    # Coverage of a nominal (1 - level)% confidence interval
    cover <- 1 / nsim * sum(true >= data[[estvarname]] - crit * data[[se]] & true <= data[[estvarname]] + crit * data[[se]], na.rm = na.rm)
    # Bias-corrected coverage of a nominal (1 - level)% confidence interval
    bccover <- 1 / nsim * sum(mean(data[[estvarname]], na.rm = na.rm) >= data[[estvarname]] - crit * data[[se]] & mean(data[[estvarname]], na.rm = na.rm) <= data[[estvarname]] + crit * data[[se]], na.rm = na.rm)
    # Power of a significance test at the `level` level
    power <- 1 / nsim * sum(abs(data[[estvarname]]) >= crit * data[[se]], na.rm = na.rm)

    ### Compute Monte Carlo SEs if requested:
    if (mcse) {
      bias_mcse <- sqrt(1 / (nsim * (nsim - 1)) * sum((data[[estvarname]] - mean(data[[estvarname]], na.rm = na.rm))^2, na.rm = na.rm))
      empse_mcse <- empse / sqrt(2 * (nsim - 1))
      mse_mcse <- sqrt(sum(((data[[estvarname]] - true)^2 - mse)^2, na.rm = na.rm) / (nsim * (nsim - 1)))
      if (!is.null(empse_ref) & !is.null(rho)) {
        relprec_mcse <- 2 * (empse_ref)^2 / (empse)^2 * sqrt((1 - rho^2) / (nsim - 1))
      } else {
        relprec_mcse <- NA
      }
      names(relprec_mcse) <- NULL
      modelse_mcse <- sqrt(se2_var / (4 * nsim * modelse^2))
      relerror_mcse <- 100 * (modelse / empse) * sqrt(se2_var / (4 * nsim * modelse^4) + 1 / (2 * nsim - 1))
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
    obj$est <- c(
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
