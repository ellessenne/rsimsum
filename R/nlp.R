# library(rsimsum)
# library(ggplot2)
# data("relhaz", package = "rsimsum")
# s1 <- simsum(data = relhaz, estvarname = "theta", se = "se", true = -0.50,
# 	methodvar = "model", by = c("n", "baseline"), x = TRUE)
#
# xx = get_data(s1, stats = "bias")
#
# a = lapply(X = s1$by, FUN = function(x) levels(xx[[x]]))
# names(a) = s1$by
#
# dgms = do.call(expand.grid, a)
# dgms[[".scenario"]] = seq(nrow(dgms))
#
# xx = merge(xx, dgms)
# xx = xx[order(xx[[".scenario"]]),]
#
# limits = range(xx[["est"]], na.rm = TRUE)
# delta = diff(range(xx$est)) / 10
#
# top = T
#
# placement = vector(mode = "list", length = length(s1$by))
#
# for (i in seq_along(placement)) {
# 	if (i == 1) {
# 		if (top) {
# 			placement[[i]] = c(round(limits[2], digits = 2) + delta, round(limits[2], digits = 2) + 2 * delta)
# 		} else {
# 			placement[[i]] = c(round(limits[1], digits = 2) - 2 * delta, round(limits[1], digits = 2) - delta)
# 		}
# 	} else {
# 		if (top) {
# 			placement[[i]] = c(placement[[i - 1]][2] + delta, placement[[i - 1]][2] + 2 * delta)
# 		} else {
# 			placement[[i]] = c(placement[[i - 1]][1] - 2 * delta, placement[[i - 1]][1] - delta)
# 		}
# 	}
# }
#
# placement
#
#
# for (i in seq_along(s1$by)) {
# 	xx[[paste0(".", s1$by[i])]] = scales::rescale(x = as.numeric(xx[[s1$by[i]]]), to = placement[[i]])
# }
#
#
#
# methodvar <- rlang::sym(s1$methodvar)
#
#
# p = xx %>%
# 	ggplot(aes(x = .scenario, y = est, group = !!methodvar)) +
# 	geom_step(aes(color = !!methodvar))
#
# for (i in seq_along(s1$by)) {
# 	.tmp = rlang::sym(paste0(".", s1$by[i]))
# 	p = p + geom_step(aes(y = !!.tmp)) +
# 		annotate(geom = "text", x = 1, y = placement[[i]][2] + delta / 2, label = paste0("yo: ", paste(levels(xx[[s1$by[i]]]), collapse = ", ")), hjust = 0, vjust = 0.5)
# }
#
# p
