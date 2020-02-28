library(rsimsum)
library(ggplot2)
data("tt", package = "rsimsum")
s6 <- simsum(data = tt, estvarname = "diff", se = "se", true = -1, x = TRUE)
autoplot(s6)




### All-in-one testing
devtools::document()
devtools::build_vignettes()
devtools::check()
devtools::check_win_oldrelease(quiet = TRUE)
devtools::check_win_release(quiet = TRUE)
devtools::check_win_devel(quiet = TRUE)
rhub::check_for_cran()
rhub::check_on_macos()
