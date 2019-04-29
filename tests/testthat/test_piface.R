library(yaml)
# Prep data ---------------------------------------------------------------

configFile = system.file(
    "extdata",
    "example_peps-master",
    "example_piface",
    "project_config.yaml",
    package = "BiocProject"
)

configNoPifaces = system.file(
    "extdata",
    "example_peps-master",
    "example_BiocProject",
    "project_config.yaml",
    package = "BiocProject"
)

p = pepr::Project(configFile)
pifaces = getPipelineInterfaces(p)
piface = pifaces[[1]]

pNoPifaces = pepr::Project(configNoPifaces)
pifaceNoPips = new("Config",piface[-which(names(piface) == "pipelines")])
pifaceNoProtoMappings = new("Config",piface[-which(names(piface) == "protocol_mapping")])

samplesTable = samples(p)

# Test --------------------------------------------------------------------

context("Test pipeline interface functions")

test_that("getPipelineInterfaces function returns a list", {
    expect_is(getPipelineInterfaces(p),"list")
})

test_that("getPipelineInterfaces function returns an object of 
          correct length", {
    expect_equal(length(getPipelineInterfaces(p)), 2)
})

test_that("object returned by getPipelineInterfaces contains the pipeline 
          interfaces", {
    expect_is(getPipelineInterfaces(p)[[1]], "Config")
})

test_that("getPipelineInterfaces warns and returns NULL when no piface section 
          not found", {
    expect_warning(expect_equal(getPipelineInterfaces(pNoPifaces), NULL))
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

test_that("getPipelines selects them by protocol", {
    expect_is(getPipelines(piface, names(piface[[1]])[[1]])[[1]], "Config")
})

test_that("getPipelines warns and returns NULL when no pipelines section not 
          found", {
    expect_warning(expect_equal(getPipelines(pifaceNoPips), NULL))
})

test_that("getPipelines warns and returns NULL when no pipelines match the 
          selected protocol", {
    expect_warning(expect_equal(getPipelines(piface, "XOXO"), NULL))
})

test_that("getPipelines warns and returns matching pipelines when some of the 
          protocols do not match the defined ones", {
    expect_warning(expect_is(getPipelines(piface, 
                        c("XOXO", "PROTO1", "PROTO2", "faultyProto")), "list"))
    expect_warning(expect_equal(length(getPipelines(piface, 
                        c("XOXO", "PROTO1", "PROTO2", "faultyProto"))), 2))
})

test_that("getProtocolMappings returns a list of corect length", {
    expect_is(getProtocolMappings(piface), "list")
})

test_that("getProtocolMappings warns and returns NULL when no pipelines section 
      is found", {
      expect_warning(expect_equal(getProtocolMappings(pifaceNoProtoMappings), 
                                  NULL))
})

test_that("samplesByProtocol returns a data.table", {
    for(pName in unlist(unique(samplesTable$protocol))){
        expect_is(samplesByProtocol(samplesTable, pName), "data.table")    
    }
})

test_that("samplesByProtocol errors when more than one protocol provided", {
              expect_error(samplesByProtocol(samplesTable,c("XOXO","XOXO")))
})

test_that("samplesByProtocol warns and returns data.table when no samples 
          match the protocol", {
    expect_warning(expect_is(samplesByProtocol(samplesTable,c("XOXO")), 
                             "data.table"))
})




