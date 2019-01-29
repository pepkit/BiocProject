Sys.setenv("R_TESTS" = "")
library(testthat)
library(BiocProject)

test_check("BiocProject")
