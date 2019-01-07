use "data-raw/MIsim.dta", clear
simsum b, se(se) id(dataset) methodvar(method) mcse saving("data-raw/MIsim_res_stata.dta")

use "data-raw/relhaz.dta", clear
simsum theta, se(se) id(dataset) methodvar(model) by(n baseline) mcse saving("data-raw/relhaz_res_stata.dta")
