# Test environments
* local macOS install (Mojave 10.14.3), R 3.5.3
* ubuntu (via Travis-CI), R-oldrel, R-release, and R-devel
* windows (via AppVeyor)
* win-builder (R-oldrel, R-release, and R-devel)

# R CMD check results on local macOS
0 errors | 0 warnings | 0 notes

# R CMD check results on win-builder (R-release)
0 errors | 0 warnings | 0 notes

# R CMD check results on win-builder (R-devel)
0 errors | 0 warnings | 0 notes

# R CMD check results on win-builder (R-oldrel)
0 errors | 0 warnings | 1 note

* checking DESCRIPTION meta-information ... NOTE
Author field differs from that derived from Authors@R
  Author:    'Alessandro Gasparini [aut, cre] (<https://orcid.org/0000-0002-8319-7624>), Ian R. White [aut]'
  Authors@R: 'Alessandro Gasparini [aut, cre] (0000-0002-8319-7624), Ian R. White [aut]'

I am not sure why the first note appears only on R-oldrel, and could not find a way to fix it. 

# Reverse dependencies
There are no reverse dependencies
