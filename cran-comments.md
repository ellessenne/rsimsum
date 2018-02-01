This is a resubmission.

> Description: Summarise results from simulation studies and compute Monte
>    Carlo standard errors of commonly used summary statistics.
>    This package is modelled  on the 'simsum' user-written
>    command in 'Stata' (See White I.R., 2010
>    <www.stata-journal.com/article.html?article=st0200>).

> Thanks, please prepend this by the protocol, i.e. use the full URL in the form <http......>.

I added the http prefix to the URL.

> We also see:
> 
> Found the following (possibly) invalid URLs:
>    URL: 
> [https://doi.org/10.1002/(SICI)1097-0258(19990330)18:6<681::AID-SIM71>3.0.CO;2-R
>      From: man/MIsim.Rd
>      Message: Invalid URI scheme
> 
> Please use \doi{} markup. If that is not helpful, you have to URLencode the DOI as "<" and ">" are invalid here ...

I removed the reference that was causing problems, as it is not of primary interest.

Thanks for the comments!

# Test environments
* local macOS (High Sierra) install, R 3.4.3
* local Windows 7 install, R 3.4.3
* ubuntu (via travis-ci), R-oldrel, R-release, and R-devel
* windows (via appveyor)
* win-builder

# R CMD check results on local macOS
0 errors | 0 warnings | 0 notes

# R CMD check results on local Windows 7
0 errors | 0 warnings | 0 notes

# Reverse dependencies
There are no reverse dependencies

# NOTE on win-builder
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Alessandro Gasparini <ag475@leicester.ac.uk>'

New submission
