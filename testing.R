library(tidyverse)
devtools::load_all()
data("nlp", package = "rsimsum")

nlp.subset <- nlp %>%
  dplyr::filter(!(ss == 100 & esigma == 2))

s.nlp.subset <- rsimsum::simsum(
  data = nlp.subset,
  estvarname = "b",
  true = 0,
  se = "se",
  methodvar = "model",
  by = c("baseline", "ss", "esigma")
)
# Okay

# But this is not okay:
autoplot(s.nlp.subset, stats = "bias", type = "nlp")

#
data("MIsim", package = "rsimsum")
s <- simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method", ref = "CC", x = TRUE)

data("frailty", package = "rsimsum")
ms <- multisimsum(
  data = frailty,
  par = "par", true = c(trt = -0.50, fv = 0.75),
  estvarname = "b", se = "se", methodvar = "model",
  by = "fv_dist",
  x = TRUE
)
ms
autoplot(ms, par = "trt", type = "zip", zip_ci_colours = c("green", "red", "yellow"))
