#' nsim
#'
#' @title Compute number of simulations required
#'
#' @description The function `nsim` computes the number of simulations \eqn{B}{B} to perform based on the accuracy of an estimate of interest, using the following equation:
#' \deqn{B = \left( \frac{(Z_{1 - \alpha / 2} + Z_{1 - theta}) \sigma}{\delta} \right) ^ 2,}{B = [((Z(1 - \alpha / 2) + Z(1 - \theta)) \sigma) / \delta] ^ 2 }
#' where \eqn{\delta} is the specified level of accuracy of the estimate of interest you are willing to accept (i.e. the permissible difference from the true value \eqn{\beta}{\beta}), \eqn{Z_{1 - \alpha / 2}}{Z(1 - \alpha / 2)} is the \eqn{(1 - \alpha / 2)}{(1 - \alpha / 2)} quantile of the standard normal distribution, \eqn{Z_{1 - \theta}}{Z(1 - \theta)} is the \eqn{(1 - \theta)}{(1 - \theta)} quantile of the standard normal distribution with \eqn{(1 - \theta)}{1 - \theta} being the power to detect a specific difference from the true value as significant, and \eqn{\sigma ^ 2}{\sigma ^ 2]} is the variance of the parameter of interest.
#'
#' @param alpha Significance level. Must be a value between 0 and 1.
#' @param sigma Variance for the parameter of interest.
#' @param delta Specified level of accuracy of the estimate of interest you are willing to accept.
#' @param power Power to detect a specific difference from the true value as significant. Must be a value between 0 and 1. Defaults to 0.5, e.g. a power of 50\%.
#'
#' @return A scalar value \eqn{B}{B} representing the number of simulations to perform based on the accuracy required.
#' @export
#'
#' @references Burton, A., Douglas G. Altman, P. Royston. et al. 2006. The design of simulation studies in medical statistics. Statistics in Medicine 25: 4279-4292 [https://doi.org/10.1002/sim.2673](https://doi.org/10.1002/sim.2673)
#'
#' @examples
#' # Number of simulations required to produce an estimate to within 5%
#' # accuracy of the true coefficient of 0.349 with a 5% significance level,
#' # assuming the variance of the estimate is 0.0166 and 50% power:
#' nsim(alpha = 0.05, sigma = sqrt(0.0166), delta = 0.349 * 5 / 100, power = 0.5)
#'
#' # Number of simulations required to produce an estimate to within 1%
#' # accuracy of the true coefficient of 0.349 with a 5% significance level,
#' # assuming the variance of the estimate is 0.0166 and 50% power:
#' nsim(alpha = 0.05, sigma = sqrt(0.0166), delta = 0.349 * 1 / 100, power = 0.5)

nsim <- function(alpha, sigma, delta, power = 0.5) {
	### Check arguments
	arg_checks = checkmate::makeAssertCollection()

	# `alpha` and `power` must be a numeric value between 0 and 1
	checkmate::assert_number(alpha, lower = 0, upper = 1, add = arg_checks)
	checkmate::assert_number(power, lower = 0, upper = 1, add = arg_checks)

	# `sigma` and `delta` must be a numeric value
	checkmate::assert_number(sigma, add = arg_checks)
	checkmate::assert_number(delta, add = arg_checks)

	### Report if there are any errors
	if (!arg_checks$isEmpty()) checkmate::reportAssertions(arg_checks)

	### Compute B
	B = (((qnorm(1 - alpha / 2) + qnorm(power)) * sigma) / delta) ^ 2
	return(B)
}
