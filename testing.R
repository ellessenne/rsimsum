set.seed(29364)
devtools::load_all()
data(tt, package = "rsimsum")

tt$true <- -1
tt2 <- dplyr::bind_rows(
  dplyr::mutate(tt, par = "diff1"),
  dplyr::mutate(tt, diff = diff + 1, true = true + 1, par = "diff2")
)

m1 <- multisimsum(data = tt2, estvarname = "diff", par = "par", true = c(diff1 = -1, diff2 = 0), se = "se", ci.limits = c("lower", "upper"), methodvar = "method", by = "dgm")
summary(m1, stats = "cover")

m2 <- multisimsum(data = tt2, estvarname = "diff", par = "par", true = "true", se = "se", ci.limits = c("lower", "upper"), methodvar = "method", by = "dgm")
summary(m2, stats = "cover")

all.equal(get_data(m1), get_data(m2))
# Should be ok!

### All-in-one testing
devtools::document()
devtools::build_vignettes()
devtools::check()
devtools::check_win_oldrelease(quiet = TRUE)
devtools::check_win_release(quiet = TRUE)
devtools::check_win_devel(quiet = TRUE)
rhub::check_for_cran()
rhub::check_on_macos()
