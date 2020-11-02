library(yaml)
# Prep data ---------------------------------------------------------------

branch = "master"

configFile = system.file(
    "extdata",
    paste0("example_peps-", branch),
    "example_piface",
    "project_config.yaml",
    package = "BiocProject"
)

configNoPifaces = system.file(
    "extdata",
    paste0("example_peps-", branch),
    "example_BiocProject",
    "project_config.yaml",
    package = "BiocProject"
)

p = pepr::Project(configFile)
pifaces = gatherPipelineInterfaces(p)
piface = pifaces[[1]]

pNoPifaces = pepr::Project(configNoPifaces)

samplesTable = sampleTable(p)

# Test --------------------------------------------------------------------

context("Test gatherPipelineInterfaces function")

test_that("gatherPipelineInterfaces function returns a character", {
    expect_is(gatherPipelineInterfaces(p),"character")
})

test_that("gatherPipelineInterfaces function returns an object of 
          correct length", {
              expect_equal(length(gatherPipelineInterfaces(p)), 2)
          })

test_that("gatherPipelineInterfaces returns NULL when no piface section
          not found", {
              expect_null(gatherPipelineInterfaces(pNoPifaces))
          })

test_that("gatherPipelineInterfaces works for project with no pipeline interfaces defined", {
    expect_warning(expect_null(gatherPipelineInterfaces(pNoPifaces, projectLevel=TRUE)))
})

context("Test output getters")

test_that("getProjectOutputs function returns a list", {
    expect_is(getProjectOutputs(p), "list")
})

test_that("getOutputsBySample function returns a list", {
    expect_is(getOutputsBySample(p), "list")
})

test_that("getOutputsBySample function allows for specific sample selection", {
    expect_false(length(getOutputsBySample(p)) == 
                     length(getOutputsBySample(p, sampleNames="sample1")))
})

test_that("getOutputsBySample errors when non-existent sample selected", {
    expect_error(getOutputsBySample(p, samleNames="bogusSample"))
})