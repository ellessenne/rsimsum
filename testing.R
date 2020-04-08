library(rsimsum)
library(ggplot2)
data("tt", package = "rsimsum")

s <- simsum(data = tt, estvarname = "diff", methodvar = "method", se = "se", true = -1, x = TRUE)
kable(s)
kable(s, stats = "bias")
kable(summary(s), stats = "bias")

class(kable(s))
class(kable(s, format = "latex"))

data("frailty", package = "rsimsum")
ms <- multisimsum(
  data = frailty,
  par = "par", true = c(trt = -0.50, fv = 0.75),
  estvarname = "b", se = "se", methodvar = "model",
  by = "fv_dist"
)
kable(ms)
kable(ms, stats = "bias")
kable(summary(ms), stats = "bias")

### All-in-one testing
devtools::document()
devtools::build_vignettes()
devtools::check()
devtools::check_win_oldrelease(quiet = TRUE)
devtools::check_win_release(quiet = TRUE)
devtools::check_win_devel(quiet = TRUE)
rhub::check_for_cran()
rhub::check_on_macos()
