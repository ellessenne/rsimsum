### working algorithm for refactoring
data("relhaz", package = "rsimsum")
relhaz2 = dplyr::bind_rows(relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz, relhaz)
relhaz3 = dplyr::bind_rows(relhaz2, relhaz2, relhaz2, relhaz2, relhaz2, relhaz2, relhaz2, relhaz2, relhaz2, relhaz2, relhaz2, relhaz2, relhaz2, relhaz2, relhaz2, relhaz2, relhaz2, relhaz2, relhaz2, relhaz2, relhaz2)
relhaz4 = dplyr::bind_rows(relhaz3, relhaz3, relhaz3, relhaz3, relhaz3)

old = simsum(data = relhaz4, estvarname = "theta", se = "se", methodvar = "model", by = c("n", "baseline"), true = 0)
new = simsum_ref(data = relhaz4, estvarname = "theta", se = "se", methodvar = "model", by = c("n", "baseline"), true = 0)


all.equal(old$summ[,1:3], new$summ[,1:3])

data("MIsim")
x <- simsum(
	data = MIsim, estvarname = "b", true = 0.5, se = "se",
	methodvar = "method", control = list(mcse = F))
xs <- summary(x, stats = c("bias", "cover", "mse"))
xs


data("relhaz", package = "rsimsum")
x = simsum(data = relhaz, estvarname = "theta", true = -0.5, se = "se", methodvar = "model", by = c("n", "baseline"))
xs = summary(x)
xs
x = xs
digits = 3
mcse = T
