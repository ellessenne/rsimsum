### Split 'data' by what is in 'by'
#' @keywords internal
.split_by <- function(data, by) split(x = data, f = lapply(X = by, FUN = function(f) data[[f]]), sep = "~")

### Bind rows of data.frame objects contained in 'x', with 'x' generally a list
#' @keywords internal
.br <- function(x) do.call(rbind.data.frame, x)

### Factorise all 'cols' in 'data' (if not already a factor)
#' @keywords internal
.factorise <- function(data, cols) {
  for (c in cols) {
    if (!is.factor(data[[c]])) data[[c]] <- factor(data[[c]])
  }
  return(data)
}

### Validate levels of 'cols' in 'data', cannot contain the character 'char'
#' @keywords internal
.validate_levels <- function(data, cols, char) {
  for (c in cols) {
    if (any(grepl(char, levels(data[[c]])))) stop(paste0("Levels of the column '", c, "' contain the forbidden character '", char, "'"))
  }
}

### Set to NA if standardised values of 'estvarname' or 'se' > 'max' (grouped by 'methodvar', 'by') using either normal ((x - mean) / stdev) or robust ((x - median) / IQR) standardisation
#' @keywords internal
.dropbig <- function(data, estvarname, se, methodvar, by, max, semax, robust, internal = TRUE) {
  splt_c <- c(methodvar, by)
  if (length(splt_c) > 0) {
    idata <- .split_by(data, splt_c)
  } else {
    idata <- list(data)
  }
  odata <- lapply(seq_along(idata), function(i) {
    tmp <- idata[[i]]
    if (robust) {
      tmp[[paste0(".", estvarname, ".std")]] <- (tmp[[estvarname]] - median(tmp[[estvarname]], na.rm = TRUE)) / (fivenum(tmp[[estvarname]], na.rm = TRUE)[4] - fivenum(tmp[[estvarname]], na.rm = TRUE)[2])
      tmp[[paste0(".", se, ".std")]] <- (tmp[[se]] - median(tmp[[se]], na.rm = TRUE)) / (fivenum(tmp[[se]], na.rm = TRUE)[4] - fivenum(tmp[[se]], na.rm = TRUE)[2])
    } else {
      tmp[[paste0(".", estvarname, ".std")]] <- (tmp[[estvarname]] - mean(tmp[[estvarname]], na.rm = TRUE)) / sqrt(var(tmp[[estvarname]], na.rm = TRUE))
      tmp[[paste0(".", se, ".std")]] <- (tmp[[se]] - mean(tmp[[se]], na.rm = TRUE)) / sqrt(var(tmp[[se]], na.rm = TRUE))
    }
    tmp
  })
  odata <- .br(odata)
  if (internal) {
    odata[[estvarname]] <- ifelse(abs(odata[[paste0(".", estvarname, ".std")]]) > max, NA, odata[[paste0(".", estvarname, ".std")]])
    odata[[se]] <- ifelse(abs(odata[[paste0(".", se, ".std")]]) > semax, NA, odata[[paste0(".", se, ".std")]])
    odata[[paste0(".", estvarname, ".std")]] <- NULL
    odata[[paste0(".", se, ".std")]] <- NULL
    return(odata)
  } else {
    .toNA <- ifelse(abs(odata[[paste0(".", estvarname, ".std")]]) > max | abs(odata[[paste0(".", se, ".std")]]) > semax, TRUE, FALSE)
    return(.toNA)
  }
}

### Set both est, se to NA if any of the two is NA
#' @keywords internal
.na_pair <- function(data, estvarname, se) {
  toNA <- (is.na(data[[estvarname]]) | is.na(data[[se]]))
  data[[estvarname]][toNA] <- NA
  data[[se]][toNA] <- NA
  data
}

### Merge description of summary statistics
#' @keywords internal
.describe <- function(x, ref, level) {
  description_df <- data.frame(
    stat = c("nsim", "thetamean", "thetamedian", "se2mean", "se2median", "bias", "empse", "relprec", "mse", "modelse", "relerror", "cover", "becover", "power"),
    description = c("Non-missing point estimates/standard errors", "Average point estimate", "Median point estimate", "Average standard error", "Median standard error", "Bias in point estimate", "Empirical standard error", paste("% gain in precision relative to method", ref), "Mean squared error", "Model-based standard error", "Relative % error in standard error", paste("Coverage of nominal", sprintf("%.0f%%", 100 * (level)), "confidence interval"), paste("Bias-eliminated coverage of nominal", sprintf("%.0f%%", 100 * (level)), "confidence interval"), paste("Power of", sprintf("%.0f%%", 100 * (1 - level)), "level test")),
    stringsAsFactors = FALSE
  )
  x <- merge(x, description_df, by = "stat")
  x <- x[, names(x)[names(x) != "stat"]]
  x$description <- factor(x$description, levels = c("Non-missing point estimates/standard errors", "Average point estimate", "Median point estimate", "Average standard error", "Median standard error", "Bias in point estimate", "Empirical standard error", paste("% gain in precision relative to method", ref), "Mean squared error", "Model-based standard error", "Relative % error in standard error", paste("Coverage of nominal", sprintf("%.0f%%", 100 * (level)), "confidence interval"), paste("Bias-eliminated coverage of nominal", sprintf("%.0f%%", 100 * (level)), "confidence interval"), paste("Power of", sprintf("%.0f%%", 100 * (1 - level)), "level test")))
  x <- x[order(x$description), ]
  x <- x[, c("description", "est", names(x)[!(names(x) %in% c("description", "est"))])]
  return(x)
}

### Format table of results for pretty printing
#' @keywords internal
.format <- function(x, digits, mcse) {
  x$summ$est <- ifelse(x$summ$stat == "nsim", sprintf("%.0f", x$summ$est), sprintf(paste0("%.", digits, "f"), x$summ$est))
  if (x$control$mcse) {
    x$summ$mcse <- sprintf(paste0("%.", digits, "f"), x$summ$mcse)
    x$summ$lower <- sprintf(paste0("%.", digits, "f"), x$summ$lower)
    x$summ$upper <- sprintf(paste0("%.", digits, "f"), x$summ$upper)
  }
  if (is.null(mcse)) {
    # Do nothing, will return est only
  } else if (mcse) {
    x$summ$est <- ifelse(x$summ$stat %in% c("nsim", "thetamean", "thetamedian", "se2mean", "se2median"), x$summ$est, paste0(x$summ$est, " (", x$summ$mcse, ")"))
  } else {
    x$summ$est <- ifelse(x$summ$stat %in% c("nsim", "thetamean", "thetamedian", "se2mean", "se2median"), x$summ$est, paste0(x$summ$est, " (", x$summ$lower, ", ", x$summ$upper, ")"))
  }
  x$summ[["mcse"]] <- NULL
  x$summ[["lower"]] <- NULL
  x$summ[["upper"]] <- NULL
  x$summ <- .describe(x$summ, ref = x$ref, level = x$control$level)
  return(x)
}

### Order a data.frame by a vector of columns
.order <- function(data, by) {
  data <- data[do.call(order, lapply(X = by, FUN = function(f) data[[f]])), ]
  data
}

### Bind methods side by side for results table
#' @keywords internal
.bind_methods <- function(data, by, methodvar) {
  data <- .split_by(data = data, by = methodvar)
  lhs <- lapply(X = c("Performance Measure", by), FUN = function(f) data[[1]][[f]])
  names(lhs) <- c("Performance Measure", by)
  data <- lapply(X = data, FUN = function(x) {
    tmp <- x
    row.names(tmp) <- NULL
    tmp
  })
  data <- lapply(X = seq_along(data), FUN = function(i) {
    tmp <- data[[i]]
    tmp[[methodvar]] <- NULL
    tmp[["Performance Measure"]] <- NULL
    if (!is.null(by)) {
      for (b in by) {
        tmp[[b]] <- NULL
      }
    }
    names(tmp) <- names(data)[i]
    tmp
  })
  data <- do.call(cbind, c(lhs, data))
  row.names(data) <- NULL
  data
}