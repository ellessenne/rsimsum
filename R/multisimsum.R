#' multisimsum
#'
#' @title Analyses of simulation studies with multiple estimands at once, including Monte Carlo error
#' @description `multisimsum` is an extension of [simsum()] that can handle multiple estimated parameters at once. `multisimsum` calls [simsum()] internally, each estimands at once. There is only one new argument that must be set when calling `multisimsum`: `par`, a string representing the column of `data` that identifies the different estimands.
#' @param par The name of the variable containing the methods to compare. Can be `NULL`.
#' @inheritParams simsum
#' @return An object of class `multisimsum`.
#' @export
#'
#' @examples
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
                        na.pair = TRUE) {
  ### Check new arguments not checked in 'simsim'
  arg_checks <- checkmate::makeAssertCollection()

  # `par` must be a single string value
  checkmate::assert_string(par, add = arg_checks)

  # `par` must be in `data`
  checkmate::assert_subset(par, choices = names(data), add = arg_checks)

  # `par` must not be any in (`stat`, `coef`, `mcse`, `lower`, `upper`)
  checkmate::assert_false(x = (par %in% c("stat", "coef", "mcse", "lower", "upper")))

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
      d[which(abs((d[[estvarname]] - mean(d[[estvarname]])) / sqrt(stats::var(d[[estvarname]]))) >= max), ]
    })
    names(big_estvarname) <- NULL
    big_estvarname <- do.call(rbind.data.frame, big_estvarname)
    # Identify big `se`
    big_se <- lapply(dropbig_split, function(d) {
      d[which(d[[se]] >= mean(d[[se]]) * semax), ]
    })
    names(big_se) <- NULL
    big_se <- do.call(rbind.data.frame, big_se)

    # Create new dataset with NA's instead of large `estvarname` and `se`
    data <- lapply(dropbig_split, function(d) {
      d[[estvarname]][which(abs((d[[estvarname]] - mean(d[[estvarname]])) / sqrt(stats::var(d[[estvarname]]))) >= max)] <- NA
      d[[se]][which(d[[se]] >= mean(d[[se]]) * semax)] <- NA
      d
    })
    names(data) <- NULL
    data <- do.call(rbind.data.frame, data)
  }

  ### Split data by `par`
  par_split <- split(x = data, f = lapply(par, function(p) data[[p]]))

  ### Call `simsum` on each element of `par_split`
  par_simsum <- lapply(par_split, function(d) simsum(data = d, true = true, estvarname = estvarname, se = se, methodvar = methodvar, ref = ref, df = df, dropbig = dropbig, max = max, semax = semax, level = level, by = by, mcse = mcse, sanitise = sanitise, na.rm = na.rm, na.pair = na.pair))

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

  ### Return object of class simsum
  class(obj) <- c("list", "multisimsum")
  return(obj)
}
