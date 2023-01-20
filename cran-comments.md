## Test environments

* local R installation, R 4.2.1, Intel-based macOS Monterey 12.5
* ubuntu-latest (via GitHub Actions, devel, release, oldrel)
* windows-latest (via GitHub Actions, devel, release, oldrel)
* macos-latest (via GitHub Actions, release, oldrel)
* windows (via winbuilder, devel, release, oldrel)
* rhub (with rhub::check_for_cran())
* arm64 mac (via macbuilder)
* arm64 mac (via rhub::check(platform = 'macos-m1-bigsur-release'))

## R CMD check results

The following note appears on all platforms:

❯ checking CRAN incoming feasibility ... NOTE
  Maintainer: ‘Alessandro Gasparini <alessandro@ellessenne.xyz>’
  
  New maintainer:
    Alessandro Gasparini <alessandro@ellessenne.xyz>
  Old maintainer(s):
    Alessandro Gasparini <alessandro.gasparini@ki.se>

This is okay, as I just updated my e-mail in the DESCRIPTION file.

Furthermore, I get some HTTP Error 503 from rhub::check_for_cran().
I manually checked each URL, which turned out to be okay and to load correctly on my machine.
I could not reproduce these issues locally, so I am not sure if this will show up on CRAN as well or not.
