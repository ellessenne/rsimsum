
<!-- README.md is generated from README.Rmd. Please edit that file -->
rsimsum
=======

[![Travis-CI Build Status](https://travis-ci.org/ellessenne/rsimsum.svg?branch=master)](https://travis-ci.org/ellessenne/rsimsum) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/ellessenne/rsimsum?branch=master&svg=true)](https://ci.appveyor.com/project/ellessenne/rsimsum) [![Coverage Status](https://img.shields.io/codecov/c/github/ellessenne/rsimsum/master.svg)](https://codecov.io/github/ellessenne/rsimsum?branch=master) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/rsimsum)](https://cran.r-project.org/package=rsimsum) [![CRAN\_Logs\_Badge](http://cranlogs.r-pkg.org/badges/rsimsum)](https://cran.r-project.org/package=rsimsum)

`rsimsum` is a porting of the user-written [simsum](http://www.stata-journal.com/article.html?article=st0200) command in Stata.

Installation
------------

You can install rsimsum from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("ellessenne/rsimsum")
```

Example
-------

This is a basic example which shows you how to solve a common problem:

``` r
library(rsimsum)
data(MIsim)
s = simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method")
#> `ref` was not specified, CC set as the reference
s
#> 
#> Call:
#>  simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", 
#>     methodvar = "method")
#> 
#> Method variable: method 
#>  Unique methods: CC, MI_LOGT, MI_T 
#>  Reference method: CC 
#> By factors: none
#> 
#> Monte Carlo standard errors were computed.
```

Summarising the results:

``` r
summary(s)
#> 
#> Call:
#>  simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", 
#>     methodvar = "method")
#> 
#> Method variable: method 
#>  Unique methods: CC, MI_LOGT, MI_T 
#>  Reference method: CC 
#> By factors: none
#> 
#> Summary statistics:
#> 
#>  Method = CC 
#>                                                   Coef   MCSE Lower 95% Upper 95%
#>                  Non-missing point estimates 1000.0000     NA        NA        NA
#>                  Non-missing standard errors 1000.0000     NA        NA        NA
#>                       Average point estimate    0.5168     NA        NA        NA
#>                        Median point estimate    0.5070     NA        NA        NA
#>                       Average standard error    0.0216     NA        NA        NA
#>                        Median standard error    0.0211     NA        NA        NA
#>                       Bias in point estimate    0.0168 0.0048    0.0074    0.0261
#>                     Empirical standard error    0.1511 0.0034    0.1445    0.1577
#>                           Mean squared error    0.0231 0.0011    0.0209    0.0253
#>    % gain in precision relative to method CC    0.0000 0.0000    0.0000    0.0000
#>               RMS model-based standard error    0.1471 0.0005    0.1461    0.1481
#>           Relative % error in standard error   -2.6594 2.2055   -6.9820    1.6633
#>  Coverage of nominal 95% confidence interval   94.3000 0.7332   92.8631   95.7369
#>                       Power of 5% level test   94.6000 0.7147   93.1992   96.0008
#> 
#>  Method = MI_LOGT 
#>                                                   Coef   MCSE Lower 95% Upper 95%
#>                  Non-missing point estimates 1000.0000     NA        NA        NA
#>                  Non-missing standard errors 1000.0000     NA        NA        NA
#>                       Average point estimate    0.5009     NA        NA        NA
#>                        Median point estimate    0.4969     NA        NA        NA
#>                       Average standard error    0.0182     NA        NA        NA
#>                        Median standard error    0.0172     NA        NA        NA
#>                       Bias in point estimate    0.0009 0.0042   -0.0073    0.0091
#>                     Empirical standard error    0.1320 0.0030    0.1262    0.1378
#>                           Mean squared error    0.0174 0.0009    0.0157    0.0191
#>    % gain in precision relative to method CC   31.0463 3.9375   23.3290   38.7636
#>               RMS model-based standard error    0.1349 0.0006    0.1338    0.1361
#>           Relative % error in standard error    2.2233 2.3323   -2.3480    6.7946
#>  Coverage of nominal 95% confidence interval   94.9000 0.6957   93.5365   96.2635
#>                       Power of 5% level test   96.9000 0.5481   95.8258   97.9742
#> 
#>  Method = MI_T 
#>                                                   Coef   MCSE Lower 95% Upper 95%
#>                  Non-missing point estimates 1000.0000     NA        NA        NA
#>                  Non-missing standard errors 1000.0000     NA        NA        NA
#>                       Average point estimate    0.4988     NA        NA        NA
#>                        Median point estimate    0.4939     NA        NA        NA
#>                       Average standard error    0.0179     NA        NA        NA
#>                        Median standard error    0.0169     NA        NA        NA
#>                       Bias in point estimate   -0.0012 0.0043   -0.0095    0.0071
#>                     Empirical standard error    0.1344 0.0030    0.1285    0.1403
#>                           Mean squared error    0.0181 0.0009    0.0163    0.0198
#>    % gain in precision relative to method CC   26.3682 3.8424   18.8372   33.8991
#>               RMS model-based standard error    0.1338 0.0006    0.1327    0.1350
#>           Relative % error in standard error   -0.4412 2.2695   -4.8894    4.0070
#>  Coverage of nominal 95% confidence interval   94.3000 0.7332   92.8631   95.7369
#>                       Power of 5% level test   96.3000 0.5969   95.1301   97.4699
```

References
==========

-   White, I.R. 2010. *simsum: Analyses of simulation studies including Monte Carlo error*. The Stata Journal 10(3): 369-385, [](http://www.stata-journal.com/article.html?article=st0200)
-   Morris, T.P, White, I.R. and Crowther, M.J. 2017. *Using simulation studies to evaluate statistical methods*. &lt;[arXiv:1712.03198](https://arxiv.org/abs/1712.03198)&gt;
