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
context("Test .unionList utility function")

test_that(".unionList returns correct object type", {
  expect_is(.unionList(list(a=1),list(a=2,b=2)), 'list')
})

test_that(".unionList returns list of correct length", {
    expect_equal(length(.unionList(list(a=1),list(a=2,b=2))), 2)
    expect_equal(length(.unionList(list(a=1,c=3),list(a=2,b=2))), 3)
    expect_equal(length(.unionList(list(a=1,b=3),list(c=2,d=2))), 4)
})

test_that(".unionList throws errors", {
    expect_error(.unionList(list(a=1),2))
})

context("Test .makeAbsPath utility function")

test_that(".makeAbsPath returns correct object", {
    expect_is(.makeAbsPath("~"),"character")
})

test_that(".makeAbsPath returns correct value", {
    expect_equal(.makeAbsPath("~"),Sys.getenv("HOME"))
})

context("Test .isDefined utility function")

test_that(".isDefined returns correct object", {
    expect_is(.isDefined(NA),"logical")
})

test_that(".isDefined returns correct value", {
    expect_equal(.isDefined(NA),FALSE)
    expect_equal(.isDefined(NULL),FALSE)
    expect_equal(.isDefined(configFile),TRUE)
})

context("Test .isAbsolute utility function")

test_that(".isAbsolute returns correct object", {
    expect_is(.isAbsolute("~"),"logical")
})

test_that(".isAbsolute returns correct value", {
    expect_equal(.isAbsolute("~"),TRUE)
    expect_equal(.isAbsolute("../test"),FALSE)
})

context("Test .callBiocFun untility function")

test_that(".callBiocFun catches errors", {
    expect_error(expect_error(.callBiocFun(a,list(testChar))))
    expect_warning(.callBiocFun(b,list(testChar)))
})

test_that(".callBiocFun returns correct object on success", {
    expect_is(.callBiocFun(c,list(testChar)),class(testChar))
})

test_that(".callBiocFun returns correct value on success", {
    expect_equal(.callBiocFun(c,list(testChar)),testChar)
})

test_that(".callBiocFun throws errors", {
    expect_error(.callBiocFun(a,testChar))
})

context("Test .insertPEP function")

test_that(".insertPEP returns correct object with a warning",{
    expect_warning(expect_is(.insertPEP("a",pepr::Project()),"Annotated"))
})

test_that(".insertPEP returns correct object",{
    expect_is(.insertPEP(S4Vectors::List(),pepr::Project()),"Annotated")
})

test_that(".insertPEP throws errors",{
    expect_error(.insertPEP(S4Vectors::List(),"test"))
})

context("Test BiocProject function")

test_that("BiocProject function return correct object", {
    expect_is(BiocProject(configFile),"Annotated")
})

test_that("BiocProject function works with arguments", {
    expect_is(BiocProject(configFileArgs),"Annotated")
    expect_is(BiocProject(configFileArgs, funcArgs = list(resize.width=200)), "Annotated")
})

# test_that("BiocProject function overrides the arguments specified in the config
#           file with ones that have the same names in the funcArgs list", {
#             expect_failure(expect_identical(
#                 BiocProject(configFileArgs),
#                 BiocProject(configFileArgs, funcArgs = list(resize.width = 200))
#             ))
# })

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
          when no bioconductor section found",{
    expect_warning(expect_is(BiocProject(configFileNoSection),"Project"))
})

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