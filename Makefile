.PHONY: pre_submission_test docs style revdep

pre_submission_test:
	make docs
	R -e "urlchecker::url_check()"
	R -e "devtools::check(remote = TRUE, manual = TRUE)"
	R -e "devtools::check_win_devel(quiet = TRUE)"
	R -e "devtools::check_win_release(quiet = TRUE)"
	R -e "devtools::check_win_oldrelease(quiet = TRUE)"
	R -e "rhub::check_for_cran()"
	R -e "rhub::check_on_macos()"
	make revdep
	make style

docs:
	make style
	R -e "devtools::document()"
	R -e "devtools::build_readme()"
	R -e "devtools::build_vignettes()"
	R -e "pkgdown::build_site()"

style:
	R -e "styler::style_dir(filetype = c('r', 'rmd'))"

revdep:
	R -e "revdepcheck::revdep_reset()"
	R -e "revdepcheck::revdep_check(num_workers = 4)"
	R -e "revdepcheck::revdep_reset()"
