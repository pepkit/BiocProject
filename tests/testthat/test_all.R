library(BiocProject)
# Prep data ---------------------------------------------------------------

configFile = system.file(
    "extdata",
    "example_peps-master",
    "example_BiocProject",
    "project_config.yaml",
    package = "BiocProject"
)

configFileArgs = system.file(
    "extdata",
    "example_peps-master",
    "example_BiocProject",
    "project_config_resize.yaml",
    package = "BiocProject"
)

configFileExceptions = system.file(
    "extdata",
    "example_peps-master",
    "example_BiocProject_exceptions",
    "project_config.yaml",
    package = "BiocProject"
)


# Test --------------------------------------------------------------------
context("Test utility functions/methods")

test_that(".updateList returns correct object type", {
  expect_is(.updateList(list(a=1),list(a=2,b=2)), 'list')
})

test_that(".updateList returns list of correct length", {
    expect_equal(length(.updateList(list(a=1),list(a=2,b=2))), 2)
    expect_equal(length(.updateList(list(a=1,c=3),list(a=2,b=2))), 3)
    expect_equal(length(.updateList(list(a=1,b=3),list(c=2,d=2))), 4)
})