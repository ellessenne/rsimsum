# Test environments
* local macOS install (Mojave 10.14.3), R 3.5.2
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
0 errors | 0 warnings | 2 notes

[...]
* checking DESCRIPTION meta-information ... NOTE
Author field differs from that derived from Authors@R
  Author:    'Alessandro Gasparini [aut, cre] (<https://orcid.org/0000-0002-8319-7624>), Ian R. White [aut]'
  Authors@R: 'Alessandro Gasparini [aut, cre] (0000-0002-8319-7624), Ian R. White [aut]'
[...]
* checking examples ...
** running examples for arch 'i386' ... [20s] OK
** running examples for arch 'x64' ... [23s] NOTE
Examples with CPU or elapsed time > 10s
                      user system elapsed
autoplot.multisimsum 10.39   0.08   10.61

I am not sure why the first note appears only on R-oldrel, and could not find a way to fix it. 
The second note may be related to some temporary issues with win-builder, as it does not appear with other (more recent) versions of R on win-builder.

# Reverse dependencies
There are no reverse dependencies
