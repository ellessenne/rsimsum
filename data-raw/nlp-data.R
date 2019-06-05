library(tidyverse)
library(rstpm2)
library(survival)
library(foreach)
library(simsurv)
set.seed(237856483)

### Fully factorial simulation study with several DGMs to test nested loop plot
dgms <- tidyr::crossing(
  baseline = forcats::as_factor(x = c("E", "W", "WW1", "WW2", "WW3", "WW4")),
  ss = c(100, 250, 500, 750, 1000),
  beta = 0.0,
  esigma = c(0.1, 0.5, 1.0, 1.5, 2.0)
)
dgms <- dgms %>%
  dplyr::full_join(
    # Parameters for the different baseline hazard functions
    tibble::tibble(
      baseline = forcats::as_factor(x = c("E", "W", "WW1", "WW2", "WW3", "WW4")),
      pars = list(
        list(lambda = 0.5),
        list(lambda = 0.5, gamma = 0.8),
        list(lambda = c(0.3, 0.5), gamma = c(1.5, 2.5), pmix = 0.7),
        list(lambda = c(0.5, 0.5), gamma = c(1.3, 0.7), pmix = 0.5),
        list(lambda = c(0.5, 0.3), gamma = c(2.5, 1.5), pmix = 0.3),
        list(lambda = c(0.5, 0.5), gamma = c(0.7, 1.3), pmix = 0.5)
      )
    ),
    "baseline"
  ) %>%
  dplyr::mutate(dgm = dplyr::row_number())

### B replications
B <- 100

### Make progress bar
pb <- utils::txtProgressBar(min = 0, max = B * nrow(dgms), style = 3)
pbindex <- 0

nlp <- foreach::foreach(j = seq(B), .combine = dplyr::bind_rows) %do% {
  res.in <- foreach::foreach(i = seq(nrow(dgms)), .combine = dplyr::bind_rows) %do% {
    # Simulate data
    X <- tibble::tibble(
      id = seq(dgms$ss[i]),
      trt = stats::rbinom(n = dgms$ss[i], size = 1, prob = 0.5),
      epsilon = stats::rnorm(n = dgms$ss[i], mean = 0, sd = dgms$esigma[i])
    )
    if (dgms$baseline[i] == "E") {
      S <- simsurv::simsurv(dist = "exponential", lambdas = dgms$pars[[i]][["lambda"]], x = X, betas = c(trt = dgms$beta[i], epsilon = 1), maxt = 10)
    } else if (dgms$baseline[i] == "W") {
      S <- simsurv::simsurv(dist = "weibull", lambdas = dgms$pars[[i]][["lambda"]], gammas = dgms$pars[[i]][["gamma"]], x = X, betas = c(trt = dgms$beta[i], epsilon = 1), maxt = 10)
    } else {
      S <- simsurv::simsurv(dist = "weibull", mixture = TRUE, lambdas = dgms$pars[[i]][["lambda"]], gammas = dgms$pars[[i]][["gamma"]], pmix = dgms$pars[[i]][["pmix"]], x = X, betas = c(trt = dgms$beta[i], epsilon = 1), maxt = 10, interval = c(0, 500))
    }
    out <- merge(X, S)
    out <- dplyr::rename(out, t = eventtime, d = status)
    out$dgm <- dgms$dgm[i]
    out <- dplyr::arrange(out, id)
    out$t[out$t < 1 / 365.242] <- 1 / 365.242 + rnorm(length(out$t[out$t < 1 / 365.242]), mean = 0, sd = 1e-4)
    out$t <- abs(out$t)

    # Fit models
    f1 <- summary(survival::coxph(formula = survival::Surv(time = t, event = d) ~ trt, data = out))
    f2 <- summary(rstpm2::stpm2(formula = survival::Surv(time = t, event = d) ~ trt, data = out, df = 5))

    # Collect results
    res <- tibble::tibble(
      i = j,
      dgm = unique(out$dgm),
      model = seq(2),
      b = c(f1$coefficients["trt", "coef"], f2@coef["trt", "Estimate"]),
      se = c(f1$coefficients["trt", "se(coef)"], f2@coef["trt", "Std. Error"])
    )

    # Update progress bar
    pbindex <- pbindex + 1
    utils::setTxtProgressBar(pb = pb, value = pbindex)

    # Return results
    return(res)
  }
  return(res.in)
}

rm(f1, f2, out, pb, res, res.in, S, X, B, i, j, pbindex)
gc()

### Merge DGMs info
nlp <- merge(nlp, dgms)

### Export for use in the package
usethis::use_data(nlp, overwrite = TRUE)
