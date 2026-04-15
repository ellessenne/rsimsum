.PHONY: pre_submission_test docs

pre_submission_test:
	R -e "urlchecker::url_check()"
	R -e "devtools::check(remote = TRUE, manual = TRUE)"
	R -e "devtools::check_win_devel(quiet = TRUE)"
	R -e "devtools::check_win_release(quiet = TRUE)"
	R -e "devtools::check_win_oldrelease(quiet = TRUE)"
	R -e "devtools::check_mac_release(quiet = TRUE)"
	R -e "rhub::rhub_check(platforms = c('linux', 'm1-san', 'macos', 'macos-arm64', 'windows'))"

docs:
	R -e "devtools::document()"
	R -e "devtools::build_readme()"
	R -e "devtools::build_vignettes()"
	R -e "pkgdown::build_site()"
