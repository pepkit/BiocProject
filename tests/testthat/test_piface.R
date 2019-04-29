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
