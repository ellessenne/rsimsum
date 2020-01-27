set.seed(29364)
devtools::load_all()
data(tt, package = "rsimsum")
tt$true <- -1
s1 <- simsum(data = tt, estvarname = "diff", true = "true", se = "se", ci.limits = c("lower", "upper"), methodvar = "method", by = "dgm")
s2 <- simsum(data = tt, estvarname = "diff", true = -1, se = "se", ci.limits = c("lower", "upper"), methodvar = "method", by = "dgm")

all.equal(get_data(s1), get_data(s2))

d1 <- get_data(s1)
d2 <- get_data(s2)
