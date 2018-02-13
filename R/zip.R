#' @title Draw zip plots.
#' @description The S3 method [zip()] produces a zip plot to visualise coverage. Zip plots are introduced in Morris _et al_., 2017.
#' @param obj An object used to select a method.
#' @param ... Further arguments passed to or from other methods.
#' @references Morris, T.P, White, I.R. and Crowther, M.J. 2017. Using simulation studies to evaluate statistical methods. [arXiv:1712.03198](https://arxiv.org/abs/1712.03198)
#' @return A [ggplot2::ggplot()] object that can be combined with additional `geom_*`, `scale_*`, `theme_*`, etc.
#' @export

zip <- function(obj, ...) {
  UseMethod("zip", obj)
}
