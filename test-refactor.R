data("MIsim", package = "rsimsum")
data("relhaz", package = "rsimsum")
single <- rsimsum::simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method", ref = "CC", x = T)
multi <- rsimsum::simsum(data = relhaz, estvarname = "theta", true = -0.5, se = "se", methodvar = "model", by = c("n", "baseline"), x = TRUE)
singlesum <- summary(single)
multisum <- summary(multi)


df = get_data(single)

library(tidyverse)
ggplot(filter(df, stat == "bias"), aes(x = method, y = 1, fill = est)) +
	geom_tile() +
	viridis::scale_fill_viridis()

autoplot(multi, type = "heat", stats = "bias") + viridis::scale_fill_viridis()
