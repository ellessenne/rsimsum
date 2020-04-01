library(rsimsum)
library(ggplot2)
data("tt", package = "rsimsum")
s6 <- simsum(data = tt, estvarname = "diff", se = "se", df = "df", true = -1, x = TRUE)
ds6 <- get_data(s6)
s7 <- simsum(data = tt, estvarname = "diff", se = "se", ci.limits = c("lower", "upper"), true = -1, x = TRUE)
ds7 <- get_data(s7)

all.equal(get_data(s6), get_data(s7))

### All-in-one testing
devtools::document()
devtools::build_vignettes()
devtools::check()
devtools::check_win_oldrelease(quiet = TRUE)
devtools::check_win_release(quiet = TRUE)
devtools::check_win_devel(quiet = TRUE)
rhub::check_for_cran()
rhub::check_on_macos()
