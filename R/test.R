# library(checkmate)
# library(assertthat)
#
# source("R/simsum.R")
#
# load("data/MIsim.rda")
# MIsim2 = haven::read_dta("data/MIsim2.dta")
# estvarname = "b"
# true = 0.5
# methodvar = "method"
# id = "dataset"
# se = "se"
# data = MIsim2
# max = 10
# semax = 100
# dropbig = FALSE
# level = 0.95
# by = "bycol"
# mcse = TRUE
# robust = FALSE
# modelsemethod = "rmse"
# ref = "CC"
# df = NULL
#
#
#
# if (is.null(by)) {
# 	data$by = 1
# 	by = "by"
# }
#
# a = simsum(data = MIsim2, estvarname = "b", true = 0.5, se = "se", methodvar = "method", by = c("bycol", "bycol2"))
# View(as.data.frame(a))
#
# library(ggplot2)
# ggplot(a, aes(x = method, y = bias, ymin = bias - 1.96 * bias_mcse, ymax = bias + 1.96 * bias_mcse)) + geom_point() + geom_errorbar() + facet_wrap(~by) + coord_flip()
#
# by = c("bycol", "bycol2")
# o = list()
# for (i in by) {
# 	o = split(MIsim2, MIsim2[[i]])
# }
#
# o = split(MIsim2, f = lapply(by, function(f) MIsim2[[f]]))
# oo = lapply(seq_along(o),
# 						function(i) {
# 							i = 1
# 							tmp = o[[i]]
# 	byvalues = unlist(strsplit(names(o)[i], ".", fixed = TRUE))
# 	for (i in 1:length(byvalues)) {
# 		tmp[[toupper(by[i])]] = unlist(byvalues)[i]
# 	}
# 	return(tmp)
# })
#
#
# strsplit("N1:M1", ":", fixed = TRUE)
