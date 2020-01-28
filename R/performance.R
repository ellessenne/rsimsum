#' @keywords internal
.performance <- function(data, estvarname, se, true, empse_ref = NULL, rho = NULL, ci.limits, control) {
  ### Make object to return
  obj <- list()

  ### Compute performance measures
  # Number of non-missing point estimates, standard errors:
  if (!is.null(se)) {
    nsim <- sum(!is.na(data[[estvarname]]) & !is.na(data[[se]]), na.rm = control$na.rm)
  } else {
    nsim <- sum(!is.na(data[[estvarname]]), na.rm = control$na.rm)
  }
  # Mean, median and variance of betas
  theta_mean <- mean(data[[estvarname]], na.rm = control$na.rm)
  theta_median <- stats::median(data[[estvarname]], na.rm = control$na.rm)
  theta_var <- stats::var(data[[estvarname]], na.rm = control$na.rm)
  # Mean, median and average of ses
  if (!is.null(se)) {
    se2_mean <- mean(data[[se]]^2, na.rm = control$na.rm)
    se2_median <- stats::median(data[[se]]^2, na.rm = control$na.rm)
    se2_var <- stats::var(data[[se]]^2, na.rm = control$na.rm)
  }
  # Bias
  if (!is.null(true)) {
    if (is.character(true)) {
      bias <- 1 / nsim * sum(data[[estvarname]] - data[[true]], na.rm = control$na.rm)
    } else {
      bias <- 1 / nsim * sum(data[[estvarname]] - true, na.rm = control$na.rm)
    }
  }
  # Empirical standard error
  empse <- sqrt(1 / (nsim - 1) * sum((data[[estvarname]] - mean(data[[estvarname]], na.rm = control$na.rm))^2, na.rm = control$na.rm))
  # Mean squared error
  if (!is.null(true)) {
    if (is.character(true)) {
      mse <- 1 / nsim * sum((data[[estvarname]] - data[[true]])^2, na.rm = control$na.rm)
    } else {
      mse <- 1 / nsim * sum((data[[estvarname]] - true)^2, na.rm = control$na.rm)
    }
  }
  # Relative change in precision
  if (!is.null(empse_ref) & !is.null(rho)) {
    relprec <- 100 * ((empse_ref / empse)^2 - 1)
  } else {
    relprec <- NA
  }
  names(relprec) <- NULL
  # Model-based standard error
  if (!is.null(se)) modelse <- sqrt(1 / nsim * sum(data[[se]]^2, na.rm = control$na.rm))
  # Relative error in model-based standard error
  if (!is.null(se)) relerror <- 100 * (modelse / empse - 1)
  # Compute critical value from either a normal or a t distribution
  crit <- ifelse(is.null(control$df), stats::qnorm(1 - (1 - control$level) / 2), stats::qt(1 - (1 - control$level) / 2, df = control$df))
  # Coverage of a nominal (1 - level)% confidence interval
  if (!is.null(true) & !is.null(se)) {
    if (is.null(ci.limits)) {
      if (is.character(true)) {
        cover <- 1 / nsim * sum(data[[true]] >= data[[estvarname]] - crit * data[[se]] & data[[true]] <= data[[estvarname]] + crit * data[[se]], na.rm = control$na.rm)
      } else {
        cover <- 1 / nsim * sum(true >= data[[estvarname]] - crit * data[[se]] & true <= data[[estvarname]] + crit * data[[se]], na.rm = control$na.rm)
      }
    } else {
      if (is.character(ci.limits)) {
        if (is.character(true)) {
          cover <- 1 / nsim * sum(data[[true]] >= data[[ci.limits[1]]] & data[[true]] <= data[[ci.limits[2]]], na.rm = control$na.rm)
        } else {
          cover <- 1 / nsim * sum(true >= data[[ci.limits[1]]] & true <= data[[ci.limits[2]]], na.rm = control$na.rm)
        }
      } else if (is.numeric(ci.limits)) {
        data[["lower"]] <- ci.limits[1]
        data[["upper"]] <- ci.limits[2]
        if (is.character(true)) {
          cover <- 1 / nsim * sum(data[[true]] >= data[["lower"]] & data[[true]] <= data[["upper"]], na.rm = control$na.rm)
        } else {
          cover <- 1 / nsim * sum(true >= data[["lower"]] & true <= data[["upper"]], na.rm = control$na.rm)
        }
      }
    }
  }
  # Bias-corrected coverage of a nominal (1 - level)% confidence interval
  if (!is.null(se)) becover <- 1 / nsim * sum(mean(data[[estvarname]], na.rm = control$na.rm) >= data[[estvarname]] - crit * data[[se]] & mean(data[[estvarname]], na.rm = control$na.rm) <= data[[estvarname]] + crit * data[[se]], na.rm = control$na.rm)
  # Power of a significance test at the `level` level
  if (!is.null(se)) power <- 1 / nsim * sum(abs(data[[estvarname]]) >= crit * data[[se]], na.rm = control$na.rm)

  ### Compute Monte Carlo SEs if requested:
  if (control$mcse) {
    if (!is.null(true)) bias_mcse <- sqrt(1 / (nsim * (nsim - 1)) * sum((data[[estvarname]] - mean(data[[estvarname]], na.rm = control$na.rm))^2, na.rm = control$na.rm))
    empse_mcse <- empse / sqrt(2 * (nsim - 1))
    if (!is.null(true)) {
      if (is.character(true)) {
        mse_mcse <- sqrt(sum(((data[[estvarname]] - data[[true]])^2 - mse)^2, na.rm = control$na.rm) / (nsim * (nsim - 1)))
      } else {
        mse_mcse <- sqrt(sum(((data[[estvarname]] - true)^2 - mse)^2, na.rm = control$na.rm) / (nsim * (nsim - 1)))
      }
    }
    if (!is.null(empse_ref) & !is.null(rho)) {
      relprec_mcse <- 200 * (empse_ref / empse)^2 * sqrt((1 - rho^2) / (nsim - 1))
    } else {
      relprec_mcse <- NA
    }
    names(relprec_mcse) <- NULL
    if (!is.null(se)) modelse_mcse <- sqrt(se2_var / (4 * nsim * modelse^2))
    if (!is.null(se)) relerror_mcse <- 100 * (modelse / empse) * sqrt(se2_var / (4 * nsim * modelse^4) + 1 / (2 * nsim - 1))
    if (!is.null(true) & !is.null(se)) cover_mcse <- sqrt(cover * (1 - cover) / nsim)
    if (!is.null(se)) becover_mcse <- sqrt(becover * (1 - becover) / nsim)
    if (!is.null(se)) power_mcse <- sqrt(power * (1 - power) / nsim)
  }

  # Easy thing to do is to add what can't be computed as null
  if (is.null(true)) {
    bias <- cover <- mse <- NULL
    bias_mcse <- cover_mcse <- mse_mcse <- NULL
  }
  if (is.null(se)) {
    se2_mean <- se2_median <- se2_var <- modelse <- relerror <- cover <- becover <- power <- NULL
    modelse_mcse <- relerror_mcse <- cover_mcse <- becover_mcse <- power_mcse <- NULL
  }

  ### Assemble object to return
  obj$stat <- c("nsim", "thetamean", "thetamedian", "se2mean", "se2median", "bias", "empse", "mse", "relprec", "modelse", "relerror", "cover", "becover", "power")
  if (is.null(true)) obj$stat <- obj$stat[!(obj$stat %in% c("bias", "cover", "mse"))]
  if (is.null(se)) obj$stat <- obj$stat[!(obj$stat %in% c("se2mean", "se2median", "modelse", "relerror", "cover", "becover", "power"))]
  obj$est <- c(nsim, theta_mean, theta_median, se2_mean, se2_median, bias, empse, mse, relprec, modelse, relerror, cover, becover, power)
  if (control$mcse) {
    obj$mcse <- c(rep(NA, ifelse(is.null(se), 3, 5)), bias_mcse, empse_mcse, mse_mcse, relprec_mcse, modelse_mcse, relerror_mcse, cover_mcse, becover_mcse, power_mcse)
  }
  obj <- as.data.frame(obj, stringsAsFactors = FALSE)

  ### Return object
  return(obj)
}
