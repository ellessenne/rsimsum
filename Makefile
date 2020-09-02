.PHONY: pre_submission_test

pre_submission_test:
	R -e "devtools::document()"
	R -e "devtools::build_vignettes()"
	R -e "pkgdown::build_site()"
	R -e "devtools::check()"
	R -e "devtools::check_win_devel(quiet = TRUE)"
	R -e "devtools::check_win_oldrelease(quiet = TRUE)"
	R -e "devtools::check_win_release(quiet = TRUE)"
	R -e "rhub::check_for_cran()"
