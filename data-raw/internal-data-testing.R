### Data from Stata's simsum for testing purposes
library(haven)
library(usethis)

data("MIsim", package = "rsimsum")
haven::write_dta(data = MIsim, path = "data-raw/MIsim.dta")
data("relhaz", package = "rsimsum")
relhaz$model <- as.numeric(factor(relhaz$model))
haven::write_dta(data = relhaz, path = "data-raw/relhaz.dta")

# Requires Stata-MP with terminal utilities installed
system("stata-mp -b do data-raw/internal-data-testing.do")

MIsim_res_stata <- haven::read_dta(file = "data-raw/MIsim_res_stata.dta")
relhaz_res_stata <- haven::read_dta(file = "data-raw/relhaz_res_stata.dta")

usethis::use_data(MIsim_res_stata, relhaz_res_stata, internal = TRUE, overwrite = TRUE)

file.remove("data-raw/MIsim.dta")
file.remove("data-raw/MIsim_res_stata.dta")
file.remove("data-raw/relhaz.dta")
file.remove("data-raw/relhaz_res_stata.dta")
file.remove("internal-data-testing.log")
