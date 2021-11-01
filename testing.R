devtools::load_all()

library(broom)

data("MIsim", package = "rsimsum")
s <- rsimsum::simsum(
	data = MIsim, estvarname = "b", true = 0.5, se = "se",
	methodvar = "method", x = TRUE
)

library(ggplot2)
autoplot(s, type = "lolly")

### All-in-one testing
devtools::document()
devtools::build_vignettes()
devtools::check()
devtools::check_win_oldrelease(quiet = TRUE)
devtools::check_win_release(quiet = TRUE)
devtools::check_win_devel(quiet = TRUE)
rhub::check_for_cran()
rhub::check_on_macos()
