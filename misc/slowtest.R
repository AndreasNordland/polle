suppressPackageStartupMessages(library("polle"))

if (Sys.getenv("GITHUB_RUN_ID") == "") {
    future::plan("multicore")
}

res <- testthat::test_dir("misc/slowtest")
if (NROW(res) > 0) {
  print(res)
  quit(status = 2)
}
