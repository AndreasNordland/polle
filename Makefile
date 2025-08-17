R = R --silent --no-save --no-echo

roxygen:
	@echo 'devtools::document(".")' | $(R)

install:
	@echo 'devtools::install(".", upgrade = "never")' | $(R)

check:
	@_R_CHECK_FORCE_SUGGESTS_=0 echo 'res <- rcmdcheck::rcmdcheck(".", build_args=c("--no-build-vignettes"), args=c("--ignore-vignettes"))' | $(R)

test-pkg: # tests locally installed version package
	@echo 'devtools::test(".")' | $(R)

test-slow:
	@echo 'testthat::test_dir("tests/slowtest")' | $(R)

test: test-pkg test-slow
