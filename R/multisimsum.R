#' @title Analyses of simulation studies with multiple estimands at once, including Monte Carlo error
#' @description `multisimsum` is an extension of [simsum()] that can handle multiple estimated parameters at once. `multisimsum` calls [simsum()] internally, each estimands at once. There is only one new argument that must be set when calling `multisimsum`: `par`, a string representing the column of `data` that identifies the different estimands.
#' @param par The name of the variable containing the methods to compare. Can be `NULL`.
#' @inheritParams simsum
#' @return An object of class `multisimsum`.
#' @export
#' @inherit simsum details
#' @examples
#' data("frailty", package = "rsimsum")
#' ms <- multisimsum(data = frailty, par = "par", true = c(trt = -0.50,
#'    fv = 0.75), estvarname = "b", se = "se", methodvar = "model",
#'    by = "fv_dist")
#' ms
multisimsum <- function(data,
                        par,
                        true,
                        estvarname,
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
  ### Check new arguments not checked in 'simsum'
  arg_checks <- checkmate::makeAssertCollection()

  # `par` must be a single string value
  checkmate::assert_string(x = par, add = arg_checks)

  # `par` must be in `data`
  checkmate::assert_subset(x = par, choices = names(data), add = arg_checks)

  # `par` must not be any in (`stat`, `est`, `mcse`, `lower`, `upper`)
  checkmate::assert_false(x = (par %in% c("stat", "est", "mcse", "lower", "upper")), add = arg_checks)

  # `true` must a named vector
  # its length must be equal to the number of unique elements in `par`
  # the names must be the same unique values in `par`
  checkmate::assert_named(x = true, add = arg_checks)
  checkmate::assert_true(x = (length(unique(data[[par]])) == length(true)), add = arg_checks)
  checkmate::assert_true(x = all(names(true) %in% unique(data[[par]])), add = arg_checks)

  # `max`, `semax`
  checkmate::assert_number(max, add = arg_checks)
  checkmate::assert_number(semax, add = arg_checks)

  # `dropbig`, `sanitise`, `na.pair`, `x` must be single logical value
  checkmate::assert_logical(dropbig, len = 1, add = arg_checks)
  checkmate::assert_logical(sanitise, len = 1, add = arg_checks)
  checkmate::assert_logical(na.pair, len = 1, add = arg_checks)
  checkmate::assert_logical(x, len = 1, add = arg_checks)

  ### Report if there are any errors
  if (!arg_checks$isEmpty()) {
    checkmate::reportAssertions(collection = arg_checks)
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

  ### Sanitise `par`` if required
  if (sanitise) {
    if (!is.null(par)) {
      estvarname <- gsub(
        pattern = ".",
        replacement = "",
        x = estvarname,
        fixed = TRUE
      )
    }
  }

  ### Identify and drop (if required) point estimates and standard errors that are too big
  if (dropbig) {
    # Splitting calculations by `by` factors and `par`
    dropbig_factors <- ifelse(is.null(by), par, c(par, by))
    # Split
    dropbig_split <- split(data, f = lapply(dropbig_factors, function(f) data[[f]]))
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

  ### Split data by `par`
  par_split <- split(x = data, f = lapply(par, function(p) data[[p]]))

  ### Call `simsum` on each element of `par_split`
  par_simsum <- lapply(seq_along(par_split), function(i) simsum(data = par_split[[i]], true = true[names(par_split)[i]], estvarname = estvarname, se = se, methodvar = methodvar, ref = ref, df = df, dropbig = FALSE, max = max, semax = semax, level = level, by = by, mcse = mcse, sanitise = sanitise, na.rm = na.rm, na.pair = FALSE))
  names(par_simsum) <- names(par_split)

  ### Bind summ slot from each object
  out <- lapply(seq_along(par_simsum), function(i) {
    x <- par_simsum[[i]]$summ
    x[[par]] <- names(par_simsum)[i]
    x
  })

  ### Bind summ results
  summ <- do.call(rbind.data.frame, out)

  ### Include call and other info into object to return
  obj <- list()
  obj$call <- match.call()
  obj$summ <- summ
  obj$par <- par
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
  class(obj) <- c("list", "multisimsum")
  return(obj)
}
