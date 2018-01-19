## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.align = "center", fig.height = 6, fig.width = 6,
  out.width = "66.66%"
)

## ----baseline-hazards----------------------------------------------------
exp_basehaz <- function(t, lambda = 0.5) lambda * 1 * t ^ 0
exp_weibull <- function(t, lambda = 0.5, gamma = 1.5) lambda * gamma * t ^ (gamma - 1)
curve(exp_basehaz, from = 0, to = 5, lty = 1, ylim = c(0, 2), ylab = expression(h[0](t)), xlab = "Follow-up time t")
curve(exp_weibull, from = 0, to = 5, lty = 2, add = TRUE)
legend(x = "topleft", lty = 1:2, legend = c("Exponential baseline hazard", "Weibull baseline hazard"), bty = "n")

## ----dgfun---------------------------------------------------------------
simulate_data <- function(dataset, n, baseline, params = list(), coveff = -0.50) {
  # Simulate treatment indicator variable
  x <- rbinom(n = n, size = 1, prob = 0.5)
  # Draw from a U(0,1) random variable
  u <- runif(n)
  # Simulate survival times depending on the baseline hazard
  if (baseline == "Exponential") {
    t <- -log(u) / (params$lambda * exp(x * coveff))
  } else {
    t <- (-log(u) / (params$lambda * exp(x * coveff))) ^ (1 / params$gamma)
  }
  # Make event indicator variable applying administrative censoring at t = 5
  d <- as.numeric(t < 5)
  t <- pmin(t, 5)
  # Return a data.frame
  data.frame(dataset = dataset, x = x, t = t, d = d, n = n, baseline = baseline, stringsAsFactors = FALSE)
}

## ----set-seed------------------------------------------------------------
set.seed(755353002)

## ----generate-data-------------------------------------------------------
data <- list()
data[["n = 100, baseline = Exp"]] <- lapply(
  X = 1:1000,
  FUN = simulate_data,
  n = 100,
  baseline = "Exponential",
  params = list(lambda = 0.5)
)
data[["n = 500, baseline = Exp"]] <- lapply(
  X = 1:1000,
  FUN = simulate_data,
  n = 500,
  baseline = "Exponential",
  params = list(lambda = 0.5)
)
data[["n = 100, baseline = Wei"]] <- lapply(
  X = 1:1000,
  FUN = simulate_data,
  n = 100,
  baseline = "Weibull",
  params = list(lambda = 0.5, gamma = 1.5)
)
data[["n = 500, baseline = Wei"]] <- lapply(
  X = 1:1000,
  FUN = simulate_data,
  n = 500,
  baseline = "Weibull",
  params = list(lambda = 0.5, gamma = 1.5)
)

## ----fitting-function----------------------------------------------------
fit_models <- function(data, model) {
  # Fit model
  if (model == "Cox") {
    fit <- coxph(Surv(t, d) ~ x, data = data)
  } else if (model == "Exponential") {
    fit <- survreg(Surv(t, d) ~ x, data = data, dist = "exponential")
  } else if (model == "Weibull") {
    fit <- survreg(Surv(t, d) ~ x, data = data, dist = "weibull")
  }
  # Return relevant coefficients
  data.frame(
    dataset = unique(data$dataset),
    n = unique(data$n),
    baseline = unique(data$baseline),
    theta = fit$coefficients["x"],
    se = sqrt(c(vcov(fit)["x", "x"])),
    model = model,
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

## ----run-models----------------------------------------------------------
library(survival)
results <- list()
results[["n = 100, baseline = Exp, model = Cox"]] <- do.call(
  rbind.data.frame,
  lapply(
    X = data[["n = 100, baseline = Exp"]],
    FUN = fit_models,
    model = "Cox"
  )
)
results[["n = 500, baseline = Exp, model = Cox"]] <- do.call(
  rbind.data.frame,
  lapply(
    X = data[["n = 500, baseline = Exp"]],
    FUN = fit_models,
    model = "Cox"
  )
)
results[["n = 100, baseline = Wei, model = Cox"]] <- do.call(
  rbind.data.frame,
  lapply(
    X = data[["n = 100, baseline = Wei"]],
    FUN = fit_models,
    model = "Cox"
  )
)
results[["n = 500, baseline = Wei, model = Cox"]] <- do.call(
  rbind.data.frame,
  lapply(
    X = data[["n = 500, baseline = Wei"]],
    FUN = fit_models,
    model = "Cox"
  )
)

results[["n = 100, baseline = Exp, model = Exponential"]] <- do.call(
  rbind.data.frame,
  lapply(
    X = data[["n = 100, baseline = Exp"]],
    FUN = fit_models,
    model = "Exponential"
  )
)
results[["n = 500, baseline = Exp, model = Exponential"]] <- do.call(
  rbind.data.frame,
  lapply(
    X = data[["n = 500, baseline = Exp"]],
    FUN = fit_models,
    model = "Exponential"
  )
)
results[["n = 100, baseline = Wei, model = Exponential"]] <- do.call(
  rbind.data.frame,
  lapply(
    X = data[["n = 100, baseline = Wei"]],
    FUN = fit_models,
    model = "Exponential"
  )
)
results[["n = 500, baseline = Wei, model = Exponential"]] <- do.call(
  rbind.data.frame,
  lapply(
    X = data[["n = 500, baseline = Wei"]],
    FUN = fit_models,
    model = "Exponential"
  )
)

results[["n = 100, baseline = Exp, model = Weibull"]] <- do.call(
  rbind.data.frame,
  lapply(
    X = data[["n = 100, baseline = Exp"]],
    FUN = fit_models,
    model = "Weibull"
  )
)
results[["n = 500, baseline = Exp, model = Weibull"]] <- do.call(
  rbind.data.frame,
  lapply(
    X = data[["n = 500, baseline = Exp"]],
    FUN = fit_models,
    model = "Weibull"
  )
)
results[["n = 100, baseline = Wei, model = Weibull"]] <- do.call(
  rbind.data.frame,
  lapply(
    X = data[["n = 100, baseline = Wei"]],
    FUN = fit_models,
    model = "Weibull"
  )
)
results[["n = 500, baseline = Wei, model = Weibull"]] <- do.call(
  rbind.data.frame,
  lapply(
    X = data[["n = 500, baseline = Wei"]],
    FUN = fit_models,
    model = "Weibull"
  )
)

## ----aggregate-results---------------------------------------------------
relhaz <- do.call(
  rbind.data.frame,
  results
)
row.names(relhaz) <- NULL

## ----saving, eval = FALSE------------------------------------------------
#  library(devtools)
#  use_data(relhaz, overwrite = TRUE)

## ----compute-summaries---------------------------------------------------
library(rsimsum)
s <- simsum(data = relhaz, estvarname = "theta", se = "se", true = -0.50, methodvar = "model", ref = "Cox", mcse = TRUE, by = c("n", "baseline"))
s

