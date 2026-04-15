# Compute number of simulations required

The function `nsim` computes the number of simulations \\B\\ to perform
based on the accuracy of an estimate of interest, using the following
equation: \$\$B = \left( \frac{(Z\_{1 - \alpha / 2} + Z\_{1 - theta})
\sigma}{\delta} \right) ^ 2,\$\$ where \\\delta\\ is the specified level
of accuracy of the estimate of interest you are willing to accept (i.e.
the permissible difference from the true value \\\beta\\), \\Z\_{1 -
\alpha / 2}\\ is the \\(1 - \alpha / 2)\\ quantile of the standard
normal distribution, \\Z\_{1 - \theta}\\ is the \\(1 - \theta)\\
quantile of the standard normal distribution with \\(1 - \theta)\\ being
the power to detect a specific difference from the true value as
significant, and \\\sigma ^ 2\\ is the variance of the parameter of
interest.

## Usage

``` r
nsim(alpha, sigma, delta, power = 0.5)
```

## Arguments

- alpha:

  Significance level. Must be a value between 0 and 1.

- sigma:

  Variance for the parameter of interest. Must be greater than 0.

- delta:

  Specified level of accuracy of the estimate of interest you are
  willing to accept. Must be greater than 0.

- power:

  Power to detect a specific difference from the true value as
  significant. Must be a value between 0 and 1. Defaults to 0.5, e.g. a
  power of 50%.

## Value

A scalar value \\B\\ representing the number of simulations to perform
based on the accuracy required.

## References

Burton, A., Douglas G. Altman, P. Royston. et al. 2006. The design of
simulation studies in medical statistics. Statistics in Medicine 25:
4279-4292 [doi:10.1002/sim.2673](https://doi.org/10.1002/sim.2673)

## Examples

``` r
# Number of simulations required to produce an estimate to within 5%
# accuracy of the true coefficient of 0.349 with a 5% significance level,
# assuming the variance of the estimate is 0.0166 and 50% power:
nsim(alpha = 0.05, sigma = sqrt(0.0166), delta = 0.349 * 5 / 100, power = 0.5)
#> [1] 209.4177

# Number of simulations required to produce an estimate to within 1%
# accuracy of the true coefficient of 0.349 with a 5% significance level,
# assuming the variance of the estimate is 0.0166 and 50% power:
nsim(alpha = 0.05, sigma = sqrt(0.0166), delta = 0.349 * 1 / 100, power = 0.5)
#> [1] 5235.443
```
