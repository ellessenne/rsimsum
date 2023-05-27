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
