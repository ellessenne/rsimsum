library(tidyverse)
library(ggridges)
library(hexSticker)
library(sysfonts)

set.seed(20181212)
dgm <- crossing(
  mean = rnorm(4, sd = 0.5),
  sd = runif(1)
) %>%
  arrange(mean)
dgm$dgm <- seq(nrow(dgm))
dgm <- crossing(
  dgm,
  data.frame(method = c(0.75, 1, 1.25))
)

n <- 15

data <- vector(mode = "list", length = nrow(dgm))
for (i in 1:nrow(dgm)) {
  data[[i]] <- data.frame(
    dgm = dgm$dgm[i],
    method = dgm$method[i],
    y = rnorm(n = n, mean = dgm$mean[i] * dgm$method[i], sd = dgm$sd[i])
  )
}
data <- bind_rows(data)

p <- ggplot(data, aes(x = y, y = factor(dgm), colour = factor(method), fill = factor(method))) +
  geom_density_ridges(alpha = 2 / 3, rel_min_height = 0.001) +
  theme_void() +
  theme_transparent() +
  coord_cartesian(clip = "off") +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  theme(legend.position = "none", plot.margin = margin(0, 0, 0, 0, "cm"))
p

sysfonts::font_add(family = "Sticker Font", regular = "lmmono10-italic.otf")
p_family <- "Sticker Font"

nf <- c(
  "inst/Sticker/rsimsum.png",
  "man/figures/hex.png"
)

for (w in nf) {
  print(
    sticker(
      subplot = p,
      s_x = 1,
      s_y = 1.15,
      s_width = 1.6,
      s_height = 1.2,
      package = "rsimsum",
      p_x = 1,
      p_y = 0.5,
      p_color = "#FDE725",
      p_family = p_family,
      p_size = 95,
      h_size = 0,
      h_fill = "#212529",
      #  url = "github.com/ellessenne/rsimsum",
      #  u_color = "#FDE725",
      #  u_family = u_family,
      #  u_size = 1.25,
      filename = w,
      dpi = 1200
    )
  )
}
