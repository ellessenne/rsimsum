data(tt, package = "rsimsum")
s <- simsum(data = tt, estvarname = "diff", true = -1, se = "se", ci.limits = c("lower", "upper"), methodvar = "method", by = "dgm")
summary(s, stats = "cover")

s <- simsum(data = tt, estvarname = "diff", true = -1, se = "se", methodvar = "method", by = "dgm")
summary(s, stats = "cover")
