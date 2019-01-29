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


# Test --------------------------------------------------------------------
context("Test utility .updateList function")

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



# test_that("Project throws errors", {
#   expect_error(Project(file = p@config$metadata$sample_annotation))
# })
# 
# test_that("Project creates an object of class Project", {
#   expect_is(p, 'Project')
# })
# 
# test_that("Project (loadConfig) produces a proper config file.
#           YAML read config has to consist of list elements of the same length
#           as the config processed with the Project constructor", {
#             expect_equal(unlist(lapply(config(p_yaml)$metadata,length)),
#                          unlist(lapply(yaml$metadata,length)))
#           })
# 
# context("utils")
# 
# test_that("listifyDF returns correct object type and throws errors", {
#   expect_is(.listifyDF(DF = DF), 'data.frame')
#   expect_is(.listifyDF(DF = DF)[[1]], 'list')
#   expect_error(.listifyDF(DF = 1))
# })
# 
# test_that("listifyDF does not change the dimensions", {
#   expect_equal(dim(.listifyDF(DF)), dim(DF))
# })
# 
# test_that(".expandPath returns correct object type and throws errors", {
#   expect_is(.expandPath(path = "~/UVA/"), 'character')
#   expect_error(.expandPath(1))
#   expect_error(.expandPath("~/$HOME/test/$NonExistentVar"))
# })
# 
# test_that("strformat returns correct object type and throws errors", {
#   expect_is(.strformat("{VAR1}{VAR2}_file", list(VAR1 = "hi", VAR2 = "hello")), "character")
#   expect_error(.strformat("{VAR1}{VAR2}_file", list(VAR1 = "hi")))
#   expect_error(.strformat(1))
# })
# 
# test_that("makeMetadataSectionAbsolute returns correct object type and throws errors",
#           {
#             expect_is(.makeMetadataSectionAbsolute(p@config, dirname(p@file)), 'list')
#             expect_error(.makeMetadataSectionAbsolute(p@file, 1))
#           })
# 
# test_that("makeMetadataSectionAbsolute does not change the length(s) of the list",
#           {
#             expect_equal(as.numeric(lapply(p@config$metadata, length)), as.numeric(lapply(
#               .makeMetadataSectionAbsolute(p@config, dirname(p@file)), length
#             )))
#           })
# 
# test_that(".isAbsolute returns correct object type and throws errors", {
#   expect_is(.isAbsolute("/home/mjs5kd"), 'logical')
#   expect_error(.isAbsolute(1))
# })
# 
# test_that(".isAbsolute works properly", {
#   expect_true(.isAbsolute("/home/mjs5kd"))
#   expect_false(.isAbsolute("UVA/data"))
# })
# 
# test_that("printNestedList throws errors", {
#   expect_error(.printNestedList(1))
# })
# 
# context("Project operations")
# 
# test_that("getSubsample method throws errors", {
#   expect_error(getSubsample(mtcars))
#   expect_error(getSubsample(p, "frog_1", "test"))
# })
# 
# test_that("getSubsample method returns a correct size DF", {
#   expect_equal(dim(getSubsample(p_sub, "frog_1", "sub_a")), c(1, 4))
# })
# 
# test_that(".loadSampleAnnotation returns a Project object", {
#   expect_is(.loadSampleAnnotation(p), 'Project')
# })
# 
# test_that(".loadSampleAnnotation thorws an error when file not fond", {
#   expect_error(.loadSampleAnnotation(p_file_missing))
# })
# 
# test_that(".loadSamplesubnnotation always returns a Project", {
#   expect_is(.loadSampleSubannotation(p), 'Project')
#   expect_is(.loadSampleSubannotation(p_subproj2), 'Project')
#   expect_is(.loadSampleSubannotation(p_sub), 'Project')
# })
# 
# test_that(".implyColumns returns Project object", {
#   expect_is(.implyAttributes(p_implied), 'Project')
# })
# 
# test_that(".implyColumns returns Project object", {
#   expect_is(.deriveAttributes(p), 'Project')
# })
# 
# test_that(".listSubprojects internal function returns correct object type, length and throws errors",
#           {
#             expect_equal(length(.listSubprojects(p_subproj1@config)), 2)
#             expect_is(.listSubprojects(p_subproj2@config), 'character')
#             expect_null(.listSubprojects(p@config))
#             expect_error(.listSubprojects(1))
#           })
# 
# test_that("listSubprojects exported method returns correct object type, length and throws errors",
#           {
#             expect_equal(length(listSubprojects(p_subproj1)), 2)
#             expect_is(listSubprojects(p_subproj1), 'character')
#             expect_null(listSubprojects(p))
#             expect_error(listSubprojects(1))
#             expect_equal(length(listSubprojects(p_subproj1)), 2)
#           })
# 
