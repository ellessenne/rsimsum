### Data from Stata's simsum for testing purposes
library(tidyverse)
library(haven)
library(usethis)

# Load, export data to Stata
data("MIsim", package = "rsimsum")
haven::write_dta(data = MIsim, path = "data-raw/MIsim.dta")
data("relhaz", package = "rsimsum")
relhaz$model <- as.numeric(factor(relhaz$model))
haven::write_dta(data = relhaz, path = "data-raw/relhaz.dta")

# Requires Stata-MP with terminal utilities installed
system("stata-mp -b do data-raw/internal-data-testing.do")

# Process results from Stata to be comparable with R
MIsim_res_stata <- haven::read_dta(file = "data-raw/MIsim_res_stata.dta") %>%
  dplyr::select(-perfmeasnum) %>%
  dplyr::rename(stat = perfmeascode) %>%
  dplyr::filter(!(stat %in% c("bsims", "sesims")))
MIsim_res_stata_est <- dplyr::select(MIsim_res_stata, stat, bCC, bMI_T, bMI_LOGT) %>%
  tidyr::gather(key = method, value = est, 2:4) %>%
  dplyr::mutate(method = dplyr::case_when(
    method == "bCC" ~ "CC",
    method == "bMI_T" ~ "MI_T",
    method == "bMI_LOGT" ~ "MI_LOGT"
  ))
MIsim_res_stata_mcse <- dplyr::select(MIsim_res_stata, stat, bCC_mcse, bMI_T_mcse, bMI_LOGT_mcse) %>%
  tidyr::gather(key = method, value = mcse, 2:4) %>%
  dplyr::mutate(method = dplyr::case_when(
    method == "bCC_mcse" ~ "CC",
    method == "bMI_T_mcse" ~ "MI_T",
    method == "bMI_LOGT_mcse" ~ "MI_LOGT"
  ))
MIsim_res_stata <- dplyr::left_join(MIsim_res_stata_est, MIsim_res_stata_mcse, by = c("stat", "method")) %>%
  tidyr::replace_na(list(mcse = 0)) %>%
  dplyr::select(stat, est, mcse, method) %>%
  dplyr::mutate(
    est = dplyr::case_when(
      stat == "relprec" ~ 1 + ifelse(!is.na(est), est / 100, 0),
      stat %in% c("cover", "power") ~ est / 100,
      TRUE ~ est
    ),
    mcse = ifelse(stat %in% c("cover", "power", "relprec"), mcse / 100, mcse)
  )
rm(MIsim_res_stata_est, MIsim_res_stata_mcse, MIsim)

relhaz_res_stata <- haven::read_dta(file = "data-raw/relhaz_res_stata.dta") %>%
  dplyr::select(-perfmeasnum) %>%
  dplyr::rename(stat = perfmeascode) %>%
  dplyr::filter(!(stat %in% c("bsims", "sesims")))
relhaz_res_stata_est <- dplyr::select(relhaz_res_stata, stat, theta1, theta2, theta3, n, baseline) %>%
  tidyr::gather(key = model, value = est, 2:4) %>%
  dplyr::mutate(model = dplyr::case_when(
    model == "theta1" ~ "1",
    model == "theta2" ~ "2",
    model == "theta3" ~ "3"
  ))
relhaz_res_stata_mcse <- dplyr::select(relhaz_res_stata, stat, theta1_mcse, theta2_mcse, theta3_mcse, n, baseline) %>%
  tidyr::gather(key = model, value = mcse, 2:4) %>%
  dplyr::mutate(model = dplyr::case_when(
    model == "theta1_mcse" ~ "1",
    model == "theta2_mcse" ~ "2",
    model == "theta3_mcse" ~ "3"
  ))
relhaz_res_stata <- dplyr::left_join(relhaz_res_stata_est, relhaz_res_stata_mcse, by = c("stat", "model", "n", "baseline")) %>%
  tidyr::replace_na(list(mcse = 0)) %>%
  dplyr::select(stat, est, mcse, model, n, baseline) %>%
  dplyr::mutate(
    est = dplyr::case_when(
      stat == "relprec" ~ 1 + ifelse(!is.na(est), est / 100, 0),
      stat %in% c("cover", "power") ~ est / 100,
      TRUE ~ est
    ),
    n = as.character(n),
    mcse = ifelse(stat %in% c("cover", "power", "relprec"), mcse / 100, mcse)
  )
rm(relhaz_res_stata_est, relhaz_res_stata_mcse, relhaz)

# Save data for internal use
usethis::use_data(MIsim_res_stata, relhaz_res_stata, internal = TRUE, overwrite = TRUE)

file.remove("data-raw/MIsim.dta")
file.remove("data-raw/MIsim_res_stata.dta")
file.remove("data-raw/relhaz.dta")
file.remove("data-raw/relhaz_res_stata.dta")
file.remove("internal-data-testing.log")
