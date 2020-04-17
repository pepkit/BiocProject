library(yaml)
# Prep data ---------------------------------------------------------------

branch = "cfg2"

configFile = system.file(
  "extdata",
  paste0("example_peps-",branch),
  "example_piface",
  "project_config.yaml",
  package = "BiocProject"
)

configNoPifaces = system.file(
    "extdata",
    paste0("example_peps-",branch),
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
