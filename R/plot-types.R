### Forest plot
#' @keywords internal
.forest_plot <- function(data, methodvar, by, stats, ci, target, scales) {
  ### Build basic plot
  methodvar <- rlang::sym(methodvar)
  gg <- ggplot2::ggplot(data = data, ggplot2::aes(x = !!methodvar, y = est)) +
    ggplot2::geom_hline(yintercept = target, linetype = "dotted") +
    ggplot2::geom_point() +
    ggplot2::labs(y = stats)

  ### Wrap by 'by' factors if defined
  if (!is.null(by)) {
    by <- rlang::syms(by)
    gg <- gg +
      ggplot2::facet_wrap(facets = ggplot2::vars(!!!by), labeller = ggplot2::label_both, scales = scales)
  }

  ### Add confidence intervals if we are calling autoplot on a summary object
  if (ci) {
    gg <- gg +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper), width = 1 / 3)
  }

  ### Return plot
  return(gg)
}

### Lolly plot
#' @keywords internal
.lolly_plot <- function(data, methodvar, by, stats, ci, target, scales) {
  ### Build basic plot
  methodvar <- rlang::sym(methodvar)
  gg <- ggplot2::ggplot(data = data, ggplot2::aes(x = est, y = !!methodvar)) +
    ggplot2::geom_vline(xintercept = target, linetype = "dotted") +
    ggplot2::geom_segment(aes(xend = target, yend = !!methodvar)) +
    ggplot2::geom_point() +
    ggplot2::labs(x = stats)

  ### Wrap by 'by' factors if defined
  if (!is.null(by)) {
    by <- rlang::syms(by)
    gg <- gg +
      ggplot2::facet_wrap(facets = ggplot2::vars(!!!by), labeller = ggplot2::label_both, scales = scales)
  }

  ### Add confidence intervals if we are calling autoplot on a summary object
  if (ci) {
    gg <- gg +
      ggplot2::geom_point(ggplot2::aes(x = lower, y = !!methodvar), shape = 40) +
      ggplot2::geom_point(ggplot2::aes(x = upper, y = !!methodvar), shape = 41)
  }

  ### Return plot
  return(gg)
}

### Zip plot
#' @keywords internal
.zip_plot <- function(data, estvarname, se, true, methodvar, by, control, summ) {
  ### Extract overall coverage
  summ <- summ[summ$stat == "cover", ]
  summ$cover <- summ$est
  if (all(c("lower", "upper") %in% names(summ))) {
    summ$cover_lower <- summ$lower
    summ$cover_upper <- summ$upper
    summ$lower <- NULL
    summ$upper <- NULL
  } else {
    summ$cover_lower <- summ$cover - stats::qnorm(1 - (1 - 0.95) / 2) * summ$mcse
    summ$cover_upper <- summ$cover + stats::qnorm(1 - (1 - 0.95) / 2) * summ$mcse
  }
  summ$est <- NULL
  summ$stat <- NULL
  summ$mcse <- NULL

  ### Define critical value utilised to compute coverage probabilities
  if (is.null(control$df)) {
    crit <- stats::qnorm(1 - (1 - control$level) / 2)
  } else {
    crit <- stats::qt(1 - (1 - control$level) / 2, df = control$df)
  }

  ### Split data by 'methodvar', 'by'
  data <- .split_by(data = data, by = by)
  data <- lapply(data, function(x) .split_by(data = x, by = methodvar))

  ### Compute coverage for each data split
  for (i in seq_along(data)) {
    for (j in seq_along(data[[i]])) {
      data[[i]][[j]][["z"]] <- (data[[i]][[j]][[estvarname]] - true) / data[[i]][[j]][[se]]
      data[[i]][[j]][["rank"]] <- rank(abs(data[[i]][[j]][["z"]])) / max(rank(abs(data[[i]][[j]][["z"]])))
      data[[i]][[j]][["lower"]] <- data[[i]][[j]][[estvarname]] - crit * data[[i]][[j]][[se]]
      data[[i]][[j]][["upper"]] <- data[[i]][[j]][[estvarname]] + crit * data[[i]][[j]][[se]]
      data[[i]][[j]][["covering"]] <- factor(ifelse(true >= data[[i]][[j]][["lower"]] & true <= data[[i]][[j]][["upper"]], TRUE, FALSE), levels = c(FALSE, TRUE), labels = c("Coverers", "Non-coverers"))
    }
    data[[i]] <- .br(data[[i]])
  }
  data <- .br(data)

  ### Merge back summary statistics
  data <- merge(x = data, y = summ)

  ### Build plot
  gg <- ggplot(data, aes(y = rank, x = lower, color = covering)) +
    geom_segment(aes(yend = rank, xend = upper)) +
    geom_vline(xintercept = true, color = "yellow", linetype = "dashed") +
    geom_hline(aes(yintercept = cover_lower), color = "yellow", linetype = "dashed") +
    geom_hline(aes(yintercept = cover_upper), color = "yellow", linetype = "dashed") +
    labs(y = expression(paste("Fractional centile of |z| for z =", (theta[i] - theta) / SE[i])), x = paste0(100 * control$level, "% confidence intervals"), color = "") +
    theme(legend.position = "bottom")

  ### If 'by', use facet_grid; facet_wrap otherwise
  if (!is.null(by)) {
    by <- rlang::syms(by)
    methodvar <- rlang::sym(methodvar)
    gg <- gg +
      ggplot2::facet_grid(cols = ggplot2::vars(!!!by), rows = ggplot2::vars(!!!methodvar))
  } else {
    methodvar <- rlang::sym(methodvar)
    gg <- gg +
      ggplot2::facet_wrap(facets = vars(!!!methodvar))
  }

  ### Return plot
  return(gg)
}

### Method vs method; supports Bland-Altman type plots
#' @keywords internal
.vs_plot <- function(data, b, methodvar, by, fitted, scales, ba) {
  ### Identify combinations of methodvar
  cs <- t(utils::combn(x = unique(data[[methodvar]]), m = 2))
  colnames(cs) <- c("X", "Y")

  ### Split data by 'by' factors
  data_split <- .split_by(data = data, by = by)

  ### Restructure data
  internal_df <- list()
  for (i in seq_along(data_split)) {
    tmp <- list()
    for (j in seq(nrow(cs))) {
      tmp[[j]] <- data.frame(
        X = data_split[[i]][[b]][data_split[[i]][[methodvar]] == cs[j, "X"]],
        Y = data_split[[i]][[b]][data_split[[i]][[methodvar]] == cs[j, "Y"]],
        contrast = ifelse(ba, paste0(cs[j, "X"], " vs ", cs[j, "Y"]), paste0("X: ", cs[j, "X"], " vs Y: ", cs[j, "Y"])),
        row.names = NULL
      )
    }
    tmp <- .br(tmp)
    if (!is.null(by)) {
      for (byval in by) {
        tmp[[byval]] <- unique(data_split[[i]][[byval]])
      }
    }
    internal_df[[i]] <- tmp
  }
  internal_df <- .br(internal_df)

  ### if Bland-Altman type plot, replace X and Y for mean and diff
  if (ba) {
    internal_df[["mean"]] <- (internal_df[["X"]] + internal_df[["Y"]]) / 2
    internal_df[["diff"]] <- internal_df[["X"]] - internal_df[["Y"]]
    internal_df[["X"]] <- internal_df[["mean"]]
    internal_df[["Y"]] <- internal_df[["diff"]]
    internal_df[["mean"]] <- NULL
    internal_df[["diff"]] <- NULL
  }

  ### Build plot
  caption <- paste0("Comparison of variable '", b, "'")
  if (ba) caption <- paste0(caption, "; Bland-Altman type plot")
  gg <- ggplot2::ggplot(data = internal_df, ggplot2::aes(x = X, y = Y)) +
    ggplot2::geom_point() +
    ggplot2::labs(caption = caption)

  ### Add reference line, depending on the type of plot
  if (ba) {
    gg <- gg +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed")
  } else {
    gg <- gg +
      ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed")
  }

  ### if Bland-Altman type plot, fix labels
  if (ba) {
    gg <- gg +
      ggplot2::labs(x = "Mean", y = "Difference")
  }

  ### If 'by', use facet_grid; facet_wrap otherwise
  if (!is.null(by)) {
    by <- rlang::syms(by)
    gg <- gg +
      ggplot2::facet_grid(cols = ggplot2::vars(!!!by), rows = ggplot2::vars(contrast), scales = scales)
  } else {
    gg <- gg +
      ggplot2::facet_wrap(~contrast, scales = scales)
  }

  ### If 'fitted' add regression line
  if (fitted) {
    gg <- gg +
      ggplot2::geom_smooth(method = "lm")
  }

  ### Return plot
  return(gg)
}

### Ridgeline plot
#' @keywords internal
.ridge_plot <- function(data, b, methodvar, by) {
  ### Create a .dgm column
  if (!is.null(by)) {
    tmp <- lapply(by, function(x) data[[x]])
    data[[".dgm"]] <- do.call(paste, c(tmp, sep = ", "))
  } else {
    data[[".dgm"]] <- "Single DGM"
  }

  ### Build plot
  b <- rlang::sym(b)
  methodvar <- rlang::sym(methodvar)
  gg <- ggplot2::ggplot(data = data, ggplot2::aes(x = !!b, y = .dgm, color = !!methodvar, fill = !!methodvar)) +
    ggridges::geom_density_ridges(alpha = 0.25) +
    ggplot2::labs(y = "")

  ### Return plot
  return(gg)
}

### Ridgeline plot
#' @keywords internal
.heat_plot <- function(data, methodvar, by, stats) {
  ### Create a .dgm column
  if (!is.null(by)) {
    tmp <- lapply(by, function(x) data[[x]])
    data[[".dgm"]] <- do.call(paste, c(tmp, sep = ", "))
  } else {
    data[[".dgm"]] <- "Single DGM"
  }

  ### Build basic plot
  methodvar <- rlang::sym(methodvar)
  gg <- ggplot2::ggplot(data = data, ggplot2::aes(x = !!methodvar, y = .dgm, fill = est)) +
    ggplot2::geom_tile() +
    ggplot2::labs(y = "", fill = stats)

  ### Return plot
  return(gg)
}
