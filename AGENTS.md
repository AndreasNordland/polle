# polle — agent notes

CRAN R package for policy learning/evaluation. Standard R package layout
(`R/`, `man/`, `tests/testthat/`, `vignettes/`). R >= 4.1 required;
`SuperLearner` is attached via `Depends`.

## Commands (use the Makefile, not bare R CMD)

- `make roxygen` — regenerate `man/` + `NAMESPACE` from roxygen headers in `R/`.
- `make install` — `devtools::install(".", upgrade = "never")`.
- `make check` — `rcmdcheck` with `--no-build-vignettes` / `--ignore-vignettes`.
- `make test-pkg` — fast suite (`tests/testthat/`).
- `make test-slow` — slow suite in `misc/slowtest/`, via `misc/slowtest.R`.
- `make test` — both.
- Single test file:
  `R -e 'testthat::test_file("tests/testthat/test-policy_eval.R")'`.
  `tests/test-all.R` sets `options(Ncpus = 2)` and
  `data.table::setDTthreads(2)` — replicate when running ad hoc if timing matters.

## Do not edit by hand

- `NAMESPACE` — roxygen2-generated. Edit roxygen in `R/*.R`, then `make roxygen`.
- `man/*.Rd` — roxygen2-generated.
- `vignettes/*.html` and `vignettes/*.R` are committed pre-built outputs;
  `make check` does not rebuild them.

## Test layout

- `tests/testthat/` — fast, run in CI on every push/PR.
- `misc/slowtest/` — expensive tests. Run only on `main` in CI, and locally via
  `make test-slow`. `misc/` is in `.Rbuildignore`, so nothing under it ships to
  CRAN and it is not seen by `R CMD check`.
- `misc/benchmark/` — benchmarks, not tests.
- `misc/slowtest.R` calls `future::plan("multicore")` when not on GitHub Actions.

## CI

- `.github/workflows/R-CMD-check.yaml`: `ubuntu-latest` + R `release` only.
  Vignettes are skipped (`--no-build-vignettes`, `--ignore-vignettes`).
  Slow tests run only on pushes to `main`.

## Code conventions

- Heavy S3 dispatch around `policy_data`, `policy_eval`, `policy_learn`,
  `policy_object`, and nuisance model families (`g_*`, `q_*`, `c_*`). Consult
  `NAMESPACE` for the authoritative export + `S3method` list before renaming,
  removing, or adding methods.
- `data.table` is imported wholesale; code uses `data.table` idioms
  (`:=`, `.SD`, keyed tables).
- `README.md` is generated from `README.Rmd` — edit the `.Rmd`.
