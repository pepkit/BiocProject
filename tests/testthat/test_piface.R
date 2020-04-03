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
pip = getPipelines(piface)[[1]]
pipNoOutputs = new("Config", pip[-which(names(pip) == "outputs")])

samplesTable = sampleTable(p)

# Test --------------------------------------------------------------------

context("Test getPipelineInterfaces function")

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
    expect_warning(expect_null(getPipelineInterfaces(pNoPifaces)))
})

context("Test getPipelines function")

test_that("getPipelines errors when executed on a list of pifaces", {
    expect_error(getPipelines(pifaces))
})

test_that("getPipelines returns a list", {
    expect_is(getPipelines(piface),"list")
})

test_that("getPipelines returns a list of correct length", {
    expect_length(getPipelines(piface),length(piface$pipelines))
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

context("Test getProtocolMappings function")

test_that("getProtocolMappings returns a list of corect length", {
    expect_is(getProtocolMappings(piface), "list")
    expect_length(getProtocolMappings(piface), 2)
})

test_that("getProtocolMappings warns and returns NULL when no pipelines section 
      is found", {
      expect_warning(expect_equal(getProtocolMappings(pifaceNoProtoMappings), 
                                  NULL))
})

context("Test samplesByProtocol function")

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

context("Test outputsByProtocols function")

test_that("outputsByProtocols works with no protocols specified and returns 
          a list of corect length", {
    expect_is(outputsByProtocols(p), "list")
    expect_length(outputsByProtocols(p), 2)
})

test_that("outputsByProtocols searches for protocols in multiple pipeline 
          interfaces, if they exist", {
    expect_length(outputsByProtocols(p, "PROTO1"), 2)
})

test_that("outputsByProtocols works with multiple protocols", {
    expect_length(outputsByProtocols(p, c("PROTO2","PROTO1")), 2)
})

test_that("outputsByProtocols does not fail when at least one of the protocols 
          is valid", {
    expect_length(outputsByProtocols(p, c("PROTO2","XOXO")), 2)
})

test_that("outputsByProtocols errors when none of the protocols match", {
    expect_error(outputsByProtocols(p, c("faultyProto","XOXO")))
})

context("Test outputsByPipeline function")

test_that("outputsByPipeline works with no pipeline specified and returns 
          a list", {
              expect_is(outputsByPipeline(p), "list")
})

test_that("outputsByPipeline returns a list when a pipeline is specified", {
    for (i in getProtocolMappings(piface)) {
        expect_is(outputsByPipeline(p, i), "list")    
    }
})

test_that("outputsByPipeline errors if more than one pipeline is specified", {
    expect_error(outputsByPipeline(p, c("a","b")))    
})

test_that("outputsByPipeline warns and returns NULL if no matches found", {
    expect_warning(expect_null(outputsByPipeline(p, c("a"))))
})

context("Test pipeline interface related utils")

test_that(".hasPipIface detects projects with a pipeline interface section", {
    expect_true(.hasPipIface(p))
    expect_false(.hasPipIface(pNoPifaces))
})

test_that(".getOutputs returns a list", {
    expect_is(.getOutputs(pip), "list")
})

test_that(".getOutputs warns and returns NULL when no outputs section found", {
    expect_warning(expect_null(.getOutputs(pipNoOutputs)))
})



