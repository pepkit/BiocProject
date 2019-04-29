library(yaml)
# Prep data ---------------------------------------------------------------

configFile = system.file(
    "extdata",
    "example_peps-master",
    "example_piface",
    "project_config.yaml",
    package = "BiocProject"
)

p = pepr::Project(configFile)
pifaces = getPipelineInterfaces(p)
piface = pifaces[[1]]

# Test --------------------------------------------------------------------

context("Test pipeline interface functions")

test_that("getPipelineInterfaces function returns a list", {
    expect_is(getPipelineInterfaces(p),"list")
})

test_that("getPipelineInterfaces function returns an object of correct length", {
    expect_equal(length(getPipelineInterfaces(p)), 2)
})

test_that("object returned by getPipelineInterfaces contains the pipeline interfaces", {
    expect_is(getPipelineInterfaces(p)[[1]], "Config")
})

test_that("getPipelines errors when executed on a list of pifaces", {
    expect_error(getPipelines(pifaces))
})

test_that("getPipelines returns a list", {
    expect_is(getPipelines(piface),"list")
})

test_that("getPipelines returns a list of correct length", {
    expect_equal(length(getPipelines(piface)),length(piface$pipelines))
})

test_that("getPipelines returns a list of Configs (pipelines)", {
    for(c in getPipelines(piface)){
        expect_is(c,"Config")
    }
})




