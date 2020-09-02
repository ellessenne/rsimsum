## Test environments

* local R installation, R 4.0.2, macOS Catalina 10.15.6
* ubuntu (on travis-ci, devel, release, oldrelease)
* windows (via win-builder, devel, release, oldrelease)
* windows (via appveyor)
* rhub (with rhub::check_for_cran())

## R CMD check results

0 errors | 0 warnings | 1 note

* checking for future file timestamps ... NOTE
  unable to verify current time

I get this note on R-hub (and local checks) but not on win-builder (nor on CI platforms); I think it's unrelated to this package?
