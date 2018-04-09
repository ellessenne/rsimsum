### Make heat plot for JOSS manuscript

library(rsimsum)
library(ggplot2)

# Data
data("relhaz", package = "rsimsum")

# Summarise results
s <- simsum(
  data = relhaz, estvarname = "theta", true = -0.5, se = "se",
  methodvar = "model", by = c("n", "baseline")
)

# Plot
plot <- heat(s, sstat = "bias", y = "baseline", text = TRUE)
ggsave(plot, filename = "inst/JOSS/plot.png", dpi = 600, width = 6, height = 4)
