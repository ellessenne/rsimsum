devtools::load_all()

# Load library and data
library(rsimsum)
data("frailty", package = "rsimsum")

# Duplicate last row and and to data
frailty <- rbind.data.frame(frailty, frailty[(nrow(frailty) - 3):nrow(frailty), ])

# Check nsim
with(frailty, table(model, interaction(fv_dist, par)))

ms1 <- multisimsum(
  data = frailty,
  par = "par",
  true = c(trt = -0.50, fv = 0.75),
  estvarname = "b",
  se = "se",
  ref = "Cox, Gamma",
  methodvar = "model",
  by = "fv_dist"
)
ms1

### All-in-one testing
devtools::document()
devtools::build_vignettes()
devtools::check()
devtools::check_win_oldrelease(quiet = TRUE)
devtools::check_win_release(quiet = TRUE)
devtools::check_win_devel(quiet = TRUE)
rhub::check_for_cran()
rhub::check_on_macos()
