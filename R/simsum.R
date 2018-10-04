#' @title Analyses of simulation studies including Monte Carlo error
#' @description `simsum` computes performance measures for simulation studies in which each simulated data set yields point estimates by one or more analysis methods. Bias, empirical standard error and precision relative to a reference method can be computed for each method.  If, in addition, model-based standard errors are available then `simsum` can compute the average model-based standard error, the relative error in the model-based standard error, the coverage of nominal confidence intervals, and the power to reject a null hypothesis. Monte Carlo errors are available for all estimated quantities.
#' @param data A `data.frame` in which variable names are interpreted. It has to be in tidy format, e.g. each variable forms a column and each observation forms a row.
#' @param estvarname The name of the variable containing the point estimates.
#' @param true The true value of the parameter. This is used in calculations of bias and coverage.
#' @param se The name of the variable containing the standard errors of the point estimates.
#' @param methodvar The name of the variable containing the methods to compare. For instance, methods could be the models compared within a simulation study. Can be `NULL`.
#' @param ref Specifies the reference method against which relative precision will be calculated. Only useful if `methodvar` is specified.
#' @param df If specified, a t distribution with `df` degrees of freedom is used when calculating coverage and power.
#' @param dropbig Specifies that point estimates or standard errors beyond the maximum acceptable values should be dropped.
#' @param max Specifies the maximum acceptable absolute value of the point estimates, standardised to mean 0 and SD 1. Defaults to `10`.
#' @param semax Specifies the maximum acceptable value of the standard error, as a multiple of the mean standard error. Defaults to `100`.
#' @param level Specifies the confidence level for coverage and power. Defaults to `0.95`.
#' @param by A vector of variable names to compute performance measures by a list of factors. Factors listed here are the (potentially several) data-generating mechanisms used to simulate data under different scenarios (e.g. sample size, true distribution of a variable, etc.). Can be `NULL`.
#' @param mcse Reports Monte Carlo standard errors for all performance measures. Defaults to `TRUE`.
#' @param sanitise Sanitise column names passed to `simsum` by removing all dot characters (`.`), which could cause problems. Defaults to `TRUE`.
#' @param na.rm A logical value indicating whether missing values (`NA`) should be removed before the computation proceeds. Defaults to `TRUE`.
#' @param na.pair Removes estimates that have a missing standard error (and vice versa). Defaults to `TRUE`.
#' @param x Set to `TRUE` to include the `data` argument (as utilised to compute summary statistics, i.e. applying `dropbig`, `na.rm`, `na.pair`) as a slot. Defaults to `FALSE`.
#' @return An object of class `simsum`.
#' @references White, I.R. 2010. simsum: Analyses of simulation studies including Monte Carlo error. The Stata Journal 10(3): 369-385. \url{http://www.stata-journal.com/article.html?article=st0200}
#' @references Morris, T.P, White, I.R. and Crowther, M.J. 2017. Using simulation studies to evaluate statistical methods. [arXiv:1712.03198](https://arxiv.org/abs/1712.03198)
#' @export
#' @details
#' The following names are not allowed for `estvarname`, `se`, `methodvar`, `by`: `stat`, `est`, `mcse`, `lower`, `upper`.
#' Calling the function with `x = TRUE` is required to produce zip plots (e.g. via the [zip()] method). The downside is that the size of the returned object increases considerably, therefore it is set to `FALSE` by default. Please note that the `data` slot returned when `x = TRUE` is obtained according to the value of the arguments `dropbig`, `na.rm`, `na.pair`; all rows with missing values are removed via a call to [stats::na.omit()].
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
             na.rm = TRUE,
             na.pair = TRUE,
             x = FALSE) {
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

    # `dropbig`, `mcse`, `sanitise`, `na.rm` , `na.pair`, `x` must be single logical value
    checkmate::assert_logical(dropbig, len = 1, add = arg_checks)
    checkmate::assert_logical(mcse, len = 1, add = arg_checks)
    checkmate::assert_logical(sanitise, len = 1, add = arg_checks)
    checkmate::assert_logical(na.rm, len = 1, add = arg_checks)
    checkmate::assert_logical(na.pair, len = 1, add = arg_checks)
    checkmate::assert_logical(x, len = 1, add = arg_checks)

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

    # `estvarname`, `se`, `methodvar`, `by` must not be any in (`stat`, `est`, `mcse`, `lower`, `upper`)
    checkmate::assert_false(x = (estvarname %in% c("stat", "est", "mcse", "lower", "upper")))
    checkmate::assert_false(x = (se %in% c("stat", "est", "mcse", "lower", "upper")))
    if (!is.null(methodvar)) checkmate::assert_false(x = (methodvar %in% c("stat", "est", "mcse", "lower", "upper")))
    if (!is.null(by)) checkmate::assert_false(x = any(by %in% c("stat", "est", "mcse", "lower", "upper")))

    ### Report if there are any errors
    if (!arg_checks$isEmpty()) {
      checkmate::reportAssertions(arg_checks)
    }

    ### Coerce `methodvar` to character (if specified and not already string)
    if (!is.null(methodvar)) {
      if (class(data[[methodvar]]) != "character") {
        data[[methodvar]] <- as.character(data[[methodvar]])
      }
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
      # Split if `by` factors are defined
      if (is.null(by)) {
        dropbig_split <- list(data)
      } else {
        dropbig_split <- split(data, f = lapply(by, function(f) data[[f]]))
      }
      # Identify big `estvarname`
      big_estvarname <- lapply(dropbig_split, function(d) {
        d[which(abs((d[[estvarname]] - mean(d[[estvarname]], na.rm = TRUE)) / sqrt(stats::var(d[[estvarname]], na.rm = TRUE))) >= max), ]
      })
      names(big_estvarname) <- NULL
      big_estvarname <- do.call(rbind.data.frame, big_estvarname)
      # Identify big `se`
      big_se <- lapply(dropbig_split, function(d) {
        d[which(d[[se]] >= mean(d[[se]], na.rm = TRUE) * semax), ]
      })
      names(big_se) <- NULL
      big_se <- do.call(rbind.data.frame, big_se)

      # Create new dataset with NA's instead of large `estvarname` and `se`
      data <- lapply(dropbig_split, function(d) {
        d[[estvarname]][which(abs((d[[estvarname]] - mean(d[[estvarname]], na.rm = TRUE)) / sqrt(stats::var(d[[estvarname]], na.rm = TRUE))) >= max)] <- NA
        d[[se]][which(d[[se]] >= mean(d[[se]], na.rm = TRUE) * semax)] <- NA
        d
      })
      names(data) <- NULL
      data <- do.call(rbind.data.frame, data)
    }

    ### Drop estimates if SE is missing, and vice versa
    if (na.pair) {
      data[[estvarname]][is.na(data[[se]])] <- NA
      data[[se]][is.na(data[[estvarname]])] <- NA
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
            stats::cor(methodvar_split[[ref]][[estvarname]], methodvar_split[[x]][[estvarname]], use = ifelse(na.rm, "na.or.complete", "everything")),
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
              empse_ref = sqrt(stats::var(methodvar_split[[ref]][[estvarname]], na.rm = na.rm)),
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
              stats::cor(methodvar_split[[ref]][[estvarname]], methodvar_split[[x]][[estvarname]], use = ifelse(na.rm, "na.or.complete", "everything")),
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
                empse_ref = sqrt(stats::var(methodvar_split[[ref]][[estvarname]], na.rm = na.rm)),
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
    obj$na.pair <- na.pair
    if (x) {
      obj$data <- stats::na.omit(data)
    }

    ### Return object of class simsum
    class(obj) <- c("list", "simsum")
    return(obj)
  }
