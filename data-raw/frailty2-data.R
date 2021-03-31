### Split frailty model into two columns
library(tidyverse)
data("frailty", package = "rsimsum")
frailty2 <- separate(frailty, col = "model", into = c("m_baseline", "m_frailty"), sep = ", ", fill = "right")
frailty2 <- replace_na(frailty2, list(m_baseline = "", m_frailty = ""))
### Export for use in the package
usethis::use_data(frailty2, overwrite = TRUE)
