# rsimsum

[![Travis-CI Build Status](https://travis-ci.org/ellessenne/rsimsum.svg?branch=master)](https://travis-ci.org/ellessenne/rsimsum)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/ellessenne/rsimsum?branch=master&svg=true)](https://ci.appveyor.com/project/ellessenne/rsimsum)
[![Coverage Status](https://img.shields.io/codecov/c/github/ellessenne/rsimsum/master.svg)](https://codecov.io/github/ellessenne/rsimsum?branch=master)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/rsimsum)](https://cran.r-project.org/package=rsimsum)

`rsimsum` is a porting of the user-written [simsum](http://www.stata-journal.com/article.html?article=st0200) command in Stata.

# To-do

- [x] Improve documentation for `simsum` (0.0.1)
- [x] S3 `is.simsum` method (0.0.2)
- [x] S3 `summary.simsum` method (0.0.2)
- [x] S3 `print.summary.simsum` method (0.0.2)
- [x] S3 `format` method (0.0.2)
- [ ] S3 `print.simsum` method (0.0.3)
- [ ] Implement reporting of observations dropped as a results of `dropbig = TRUE` (0.0.4)
- [ ] Implement calculating (and reporting) mcse only when `mcse = TRUE` (0.0.5)
- [ ] Robust calculations (0.0.6)
- [ ] Set up TravisCI, Appveyor, Codecov (0.0.7)
- [ ] Set up testing suite: expected errors/warnings with wrong input, correct results with appropriate input (0.0.8)
- [ ] Tidy up code before 0.1.0 release
