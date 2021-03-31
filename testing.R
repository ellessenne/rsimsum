devtools::load_all()

# Load library and data
library(rsimsum)
data("MIsim2", package = "rsimsum")

s <- simsum(data = MIsim2, estvarname = "b", true = 0.5, se = "se", methodvar = c("m1", "m2"), x = T)

autoplot(summary(s), type = "zip")

### All-in-one testing
devtools::document()
devtools::build_vignettes()
devtools::check()
devtools::check_win_oldrelease(quiet = TRUE)
devtools::check_win_release(quiet = TRUE)
devtools::check_win_devel(quiet = TRUE)
rhub::check_for_cran()
rhub::check_on_macos()
