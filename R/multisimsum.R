#' @title Analyses of simulation studies with multiple estimands at once, including Monte Carlo error
#' @description `multisimsum` is an extension of [simsum()] that can handle multiple estimated parameters at once.
#' `multisimsum` calls [simsum()] internally, each estimands at once.
#' There is only one new argument that must be set when calling `multisimsum`: `par`, a string representing the column of `data` that identifies the different estimands.
#' Additionally, with `multisimsum` the argument `true` can be a named vector, where names correspond to each estimand (see examples).
#' Otherwise, constant values (or values identified by a column in `data`) will be utilised.
#' See `vignette("E-custom-inputs", package = "rsimsum")` for more details.
#' @param par The name of the variable containing the methods to compare.
#' Can be `NULL`.
#' @inheritParams simsum
#' @return An object of class `multisimsum`.
#' @export
#' @details
#' The following names are not allowed for `estvarname`, `se`, `methodvar`, `by`, `par`: `stat`, `est`, `mcse`, `lower`, `upper`.
#' @examples
#' data("frailty", package = "rsimsum")
#' ms <- multisimsum(
#'   data = frailty,
#'   par = "par", true = c(trt = -0.50, fv = 0.75),
#'   estvarname = "b", se = "se", methodvar = "model",
#'   by = "fv_dist"
#' )
#' ms
multisimsum <- function(data,
                        par,
                        estvarname,
                        se = NULL,
                        true = NULL,
                        methodvar = NULL,
                        ref = NULL,
                        by = NULL,
                        ci.limits = NULL,
                        dropbig = FALSE,
                        x = FALSE,
                        control = list()) {
  ### Check arguments
  arg_checks <- checkmate::makeAssertCollection()
  # 'methodvar', 'ref', 'par' must be a single string value
  checkmate::assert_string(x = methodvar, null.ok = TRUE, add = arg_checks)
  checkmate::assert_string(x = ref, null.ok = TRUE, add = arg_checks)
  checkmate::assert_string(x = par, add = arg_checks)
  # 'methodvar', 'par' must be in 'data'
  checkmate::assert_subset(x = methodvar, choices = names(data), add = arg_checks)
  checkmate::assert_subset(x = par, choices = names(data), add = arg_checks)
  # 'ref' must be one of the options in 'methodvar'
  if (!is.null(methodvar)) {
    checkmate::assert_subset(x = ref, choices = as.character(unique(data[[methodvar]])), add = arg_checks)
  }
  # 'methodvar', 'par' must not be any in ('stat', 'est', 'mcse', 'lower', 'upper')
  if (!is.null(methodvar)) checkmate::assert_false(x = (methodvar %in% c("stat", "est", "mcse", "lower", "upper")))
  checkmate::assert_false(x = (par %in% c("stat", "est", "mcse", "lower", "upper")), add = arg_checks)
  # 'true' can be a named vector, with numeric values
  # its length must be equal to the number of unique elements in 'par'
  # the names must be the same unique values in 'par'
  # N.B.: if not, then pass everything onto simsum
  if (!is.null(true)) {
    if (rlang::is_named(true)) {
      checkmate::assert_named(x = true, add = arg_checks)
      checkmate::assert_true(x = (length(unique(data[[par]])) == length(true)), add = arg_checks)
      checkmate::assert_true(x = all(names(true) %in% unique(data[[par]])), add = arg_checks)
    }
  }
  # 'control' must be a list, with well defined components
  checkmate::assert_list(x = control, add = arg_checks)
  checkmate::assert_subset(x = names(control), choices = c("mcse", "level", "df", "na.rm", "char.sep", "dropbig.max", "dropbig.semax", "dropbig.robust"), empty.ok = TRUE, add = arg_checks)
  checkmate::assert_logical(x = control$mcse, len = 1, null.ok = TRUE, add = arg_checks)
  checkmate::assert_number(x = control$level, lower = 0, upper = 1, null.ok = TRUE, add = arg_checks)
  checkmate::assert_number(x = control$df, null.ok = TRUE, add = arg_checks)
  checkmate::assert_logical(x = control$na.rm, len = 1, null.ok = TRUE, add = arg_checks)
  checkmate::assert_string(x = control$char.sep, null.ok = TRUE, add = arg_checks)
  checkmate::assert_number(x = control$dropbig.max, null.ok = TRUE, add = arg_checks)
  checkmate::assert_number(x = control$dropbig.semax, null.ok = TRUE, add = arg_checks)
  checkmate::assert_logical(x = control$dropbig.robust, len = 1, null.ok = TRUE, add = arg_checks)
  # Report
  if (!arg_checks$isEmpty()) checkmate::reportAssertions(arg_checks)

  ### Set control parameters
  control.default <- list(mcse = TRUE, level = 0.95, df = NULL, na.rm = TRUE, char.sep = "~", dropbig.max = 10, dropbig.semax = 100, dropbig.robust = TRUE)
  control.tmp <- unlist(list(
    control[names(control) %in% names(control.default)],
    control.default[!(names(control.default) %in% names(control))]
  ), recursive = FALSE)
  control <- control.tmp

  ### Factorise 'par', 'methodvar'
  data <- .factorise(data = data, cols = c(par, methodvar))

  ### Check that levels of factors are ok
  .validate_levels(data = data, cols = c(par, methodvar), char = ifelse(!is.null(control$char.sep), control$char.sep, "~"))

  ### Set reference method if 'ref' is not specified
  if (!is.null(methodvar)) {
    methods <- levels(data[[methodvar]])
    if (is.null(ref)) {
      ref <- methods[1]
      message(paste("'ref' method was not specified,", ref, "set as the reference"))
    }
    data[[methodvar]] <- relevel(data[[methodvar]], ref = ref)
  }

  ### Throw a warning if `ref` is specified and `methodvar` is not
  if (is.null(methodvar) & !is.null(ref)) {
    warning("'ref' method is specified while 'methodvar' is not: 'ref' will be ignored")
    ref <- NULL
  }

  ### Split data by 'par'
  par_split <- .split_by(data = data, by = par)

  ### Call 'simsum' on each element of 'par_split'; save data if 'x = TRUE'
  par_simsum <- vector(mode = "list", length = length(par_split))
  if (x) par_data <- vector(mode = "list", length = length(par_split))
  for (i in seq_along(par_split)) {
    if (rlang::is_named(true) | is.null(true)) {
      run <- simsum(data = par_split[[i]], estvarname = estvarname, true = true[names(par_split)[i]], se = se, methodvar = methodvar, ref = ref, by = by, ci.limits = ci.limits, dropbig = dropbig, x = x, control = control)
    } else {
      run <- simsum(data = par_split[[i]], estvarname = estvarname, true = true, se = se, methodvar = methodvar, ref = ref, by = by, ci.limits = ci.limits, dropbig = dropbig, x = x, control = control)
    }
    par_simsum[[i]] <- run[["summ"]]
    if (x) par_data[[i]] <- run[["x"]]
  }
  names(par_simsum) <- names(par_split)

  ### Add a column with the parameter to each slot, and turn it into factor
  for (i in seq_along(par_simsum)) par_simsum[[i]][[par]] <- names(par_simsum)[i]

  ### Bind summ slots
  summ <- .br(x = par_simsum)
  summ <- .factorise(data = summ, cols = par)
  row.names(summ) <- NULL

  ### Include stuff into object to return
  obj <- list()
  obj$summ <- summ
  obj$par <- par
  obj$estvarname <- estvarname
  obj$true <- true
  obj$se <- se
  obj$methodvar <- methodvar
  obj$ref <- ref
  obj$dropbig <- dropbig
  if (!is.null(ci.limits)) {
    obj$ci.limits <- ci.limits
  }
  obj$by <- by
  obj$control <- control
  if (x) {
    obj$x <- .br(par_data)
    rownames(obj$x) <- NULL
  }

  ### Return object of class simsum
  class(obj) <- c("list", "multisimsum")
  return(obj)
}
