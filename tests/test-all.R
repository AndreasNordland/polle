suppressPackageStartupMessages(library("testthat"))
options(Ncpus = 2)
data.table::setDTthreads(2)
test_check("polle")
