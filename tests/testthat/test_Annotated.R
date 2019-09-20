library(yaml)
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

configFileMissingFun = system.file(
    "test_projects",
    "faulty_project",
    "project_config_no_function.yaml",
    package = "BiocProject"
)

configFileNoSection = system.file(
    "test_projects",
    "faulty_project",
    "project_config_no_section.yaml",
    package = "BiocProject"
)

bp = BiocProject(configFile)

# Test --------------------------------------------------------------------

context("Test Annotated methods")

test_that("samples returns a correct object", {
    expect_is(samples(bp),"data.table")
})

test_that("config returns a correct object", {
    expect_is(config(bp),"Config")
})

test_that(".is.project returns a correct object", {
    expect_is(.is.project(bp),"logical")
})

test_that(".is.project returns a value", {
    expect_equal(.is.project(bp),TRUE)
    expect_equal(.is.project(S4Vectors::List(a=1)), FALSE)
})

test_that("is method returns correct value when Annotated provided", {
    expect_equal(is(bp,"Project"), TRUE)    
})

test_that("getProject returns a correct object", {
    expect_is(getProject(bp),"Project")
})

test_that("getProject returns a correct value", {
    expect_equal(getProject(bp), pepr::Project(configFile))
})