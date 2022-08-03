devtools::load_all()

data("MIsim", package = "rsimsum")
MIsim[["true"]] <- 0.5
s <- rsimsum::simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method")
ss <- summary(s, stats = "rbias")
ss

# compare with:
library(simhelpers)
lapply(X = split(x = MIsim, f = MIsim$method), FUN = function(x) simhelpers::calc_relative(res_dat = x, estimates = "b", true_param = "true")) %>%
  do.call(rbind.data.frame, .) %>%
  as.data.frame()
