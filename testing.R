library(tidyverse)
devtools::load_all()

# #48
library(rsimsum)
s.nlp.true <- rsimsum::simsum(
  data = nlp, estvarname = "b", true = "esigma", se = "se",
  methodvar = "model", by = c("baseline", "ss", "esigma")
)
autoplot(s.nlp.true, stats = "bias", type = "nlp")
nlp$esigma.copy <- nlp$esigma
s.nlp.true2 <- rsimsum::simsum(
  data = nlp, estvarname = "b", true = "esigma.copy", se = "se",
  methodvar = "model", by = c("baseline", "ss", "esigma")
)
autoplot(s.nlp.true2, stats = "bias", type = "nlp")

# #49
library(dplyr)
devtools::load_all()
data("nlp", package = "rsimsum")
# estvarname:
rsimsum::simsum(
  data = rename(nlp, est = b), estvarname = "est", true = 0, se = "se",
  methodvar = "model", by = c("baseline", "ss", "esigma")
)
# se:
rsimsum::simsum(
  data = rename(nlp, est = se), estvarname = "b", true = 0, se = "est",
  methodvar = "model", by = c("baseline", "ss", "esigma")
)
# methodvar:
rsimsum::simsum(
  data = rename(nlp, est = model), estvarname = "b", true = 0, se = "se",
  methodvar = "est", by = c("baseline", "ss", "esigma")
)
# by:
rsimsum::simsum(
  data = rename(nlp, est = ss), estvarname = "b", true = 0, se = "se",
  methodvar = "model", by = c("baseline", "est", "esigma")
)

# #48
devtools::load_all()
library(ggplot2)
s.nlp.true <- rsimsum::simsum(
  data = nlp, estvarname = "b", true = "esigma", se = "se",
  methodvar = "model", by = c("baseline", "ss", "esigma")
)
autoplot(s.nlp.true, stats = "bias", type = "nlp")
