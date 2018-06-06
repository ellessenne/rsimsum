#' @keywords internal
merge_description <- function(x, ref, level) {
  description_df <- data.frame(
    stat = c("nsim", "thetamean", "thetamedian", "se2mean", "se2median", "bias", "empse", "mse", "relprec", "modelse", "relerror", "cover", "bccover", "power"),
    description = c("Simulations with non-missing estimates/SEs", "Average point estimate", "Median point estimate", "Average standard error", "Median standard error", "Bias in point estimate", "Empirical standard error", "Mean squared error", paste("% gain in precision relative to method", ref), "Model-based standard error", "Relative % error in standard error", paste("Coverage of nominal", sprintf("%.0f%%", 100 * (level)), "CI"), paste("Bias corrected coverage of nominal", sprintf("%.0f%%", 100 * (level)), "CI"), paste("Power of", sprintf("%.0f%%", 100 * (1 - level)), "level test"))
  )
  x <- merge(x, description_df, by = "stat")
  return(x)
}
