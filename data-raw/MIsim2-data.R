### Split MIsim method into two columns
library(tidyverse)
data("MIsim", package = "rsimsum")
MIsim2 <- separate(MIsim, col = "method", into = c("m1", "m2"), sep = "_", fill = "right")
MIsim2 <- replace_na(MIsim2, list(m1 = "", m2 = ""))
### Export for use in the package
usethis::use_data(MIsim2, overwrite = TRUE)
