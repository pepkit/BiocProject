library(BiocProject)
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

configFileExceptions = system.file(
    "extdata",
    "example_peps-master",
    "example_BiocProject_exceptions",
    "project_config.yaml",
    package = "BiocProject"
)

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
context("Test .updateList utility function")

test_that(".updateList returns correct object type", {
  expect_is(.updateList(list(a=1),list(a=2,b=2)), 'list')
})

test_that(".updateList returns list of correct length", {
    expect_equal(length(.updateList(list(a=1),list(a=2,b=2))), 2)
    expect_equal(length(.updateList(list(a=1,c=3),list(a=2,b=2))), 3)
    expect_equal(length(.updateList(list(a=1,b=3),list(c=2,d=2))), 4)
})

test_that(".updateList throws errors", {
    expect_error(.updateList(list(a=1),2))
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
    expect_equal(.isDefined(NA),F)
    expect_equal(.isDefined(NULL),F)
    expect_equal(.isDefined(configFile),T)
})

context("Test .isAbsolute utility function")

test_that(".isAbsolute returns correct object", {
    expect_is(.isAbsolute("~"),"logical")
})

test_that(".isAbsolute returns correct value", {
    expect_equal(.isAbsolute("~"),T)
    expect_equal(.isAbsolute("../test"),F)
})

context("Test .callBiocFun untility function")

test_that(".callBiocFun catches errors", {
    expect_error(expect_error(.callBiocFun(a,list(testChar))))
    expect_equal(.callBiocFun(a,list(testChar)),testChar)
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