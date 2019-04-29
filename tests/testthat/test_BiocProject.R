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

configPiface = system.file(
    "extdata",
    "example_peps-master",
    "example_piface",
    "project_config.yaml",
    package = "BiocProject"
)


bp = BiocProject(configFile)

a=function(arg) {
    stop(arg)
}

b=function(arg) {
    warning(arg)
}

c=function(arg) {
    return(arg)
}

testChar = "a"

# Test --------------------------------------------------------------------

context("Test BiocProject function")

test_that("BiocProject function return correct object", {
    expect_is(BiocProject(configFile),"Annotated")
})

test_that("BiocProject function works with arguments", {
    expect_is(BiocProject(configFileArgs),"Annotated")
    expect_is(BiocProject(configFileArgs, funcArgs = list(resize.width=200)),
              "Annotated")
})

test_that("BiocProject function returns Annotated when provided objects of 
          different class and thorows a warning", {
    expect_warning(expect_is(BiocProject(configFile, func = function(x){
        return("test")
    }),"Annotated"))
})

test_that("BiocProject function returns a Project object 
          when autoload is set to FALSE", {
    expect_is(BiocProject(file=configFile,autoLoad = FALSE),"Project")
})

test_that("BiocProject function throws errors/warnings 
          when the arguments are inappropriate", {
    expect_error(BiocProject(file=configFile,func = "2"))
    expect_error(BiocProject(file = "test"))
    expect_error(BiocProject(file = configFile,autoLoad = "test"))
})

test_that("BiocProject function catches errors in the user-provided 
          function returns the error message as Annotated", {
    expect_is(BiocProject(file=configFile,func=function(x) {
        stop("test")
    }),"Annotated")
})

test_that("BiocProject function catches errors when the function specified
          does not exist", {
              expect_error(BiocProject(configFileMissingFun))  
          })

test_that("BiocProject function throws a warning and returns a Project object
          when no bioconductor section found", {
    expect_warning(expect_is(BiocProject(configFileNoSection),"Project"))
})

test_that("BiocProject function reads the bioconductor section from the 
          pipeline interface if not found in the project config", {
              expect_true(is(BiocProject(configPiface), "Project"))
})

test_that("BiocProject function returna a valid object when the bioconductor 
          section defined in the specific pipeline is requested", {
    expect_true(is(BiocProject(configPiface,
                               pipelineName = "other_pipeline2.py"), "Project"))
})