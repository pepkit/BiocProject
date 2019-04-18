
#' Switches from python to R list accession syntax
#' 
#' Python uses a dot to access attributes, while R uses `$`; this function
#' converts the python style into R so that we can use R code to populate
#' variables with R lists. From this: '(sample.name}' to this: '{sample$name}'
#' @param str String to recode
#' @examples
#' pytor("{sample.name}...{project.source.name}")
pytor = function(str) {
    # This is the regex where the magic happens
    pytor1 = function(str) gsub("(\\{[^\\.\\}]+)\\.", "\\1$", str)
    # This loop allows multi-layer accession
    res = str
    prev = ""
    while (prev != res) {
        prev = res
        res = pytor1(res)
    }
    return(res)
}

#' Populates a variable-encoded string with sample/project variables
#' 
#' Given a string and a project this function will go through samples and populate
#' the variables. Used to return real files for each sample from an output variable
#' in the pipeline interface
#' @param string Variable-encoded string to populate
#' @param pepr::Project object with values to draw from
#' @export
#' @examples
#' configFile = system.file(
#'   "extdata",
#'   "example_peps-master",
#'   "example_BiocProject",
#'   "project_config.yaml",
#'   package = "BiocProject"
#' )
#' bp = BiocProject(file=configFile)
#' populateString("{sample.sample_name}/{sample.file_path}", bp)
populateString = function(string, project) {
    # Apply this glue function on each row in the samples table,
    # coerced to a list object to allow attribute accession.
    populatedStrings = apply(pepr::samples(project), 1, function(s) {
        with(list(sample=s, project=project), glue::glue(pytor(string)))
    })
    return(populatedStrings)
}

#' Returns pipeline interface object
#' TODO: handle multiple pipeline interfaces
#' TODO: Load this at construction time.
getPiface = function(project) {
    if (!is.null(config(bp)$metadata$pipeline_interface)) {
        piface = yaml::yaml.load_file(pepr::config(project)$metadata$pipeline_interface)
        return(piface)
    } else {
        message("No pipeline interface found.")
        return(NULL)
    }

}

getOutputs = function(project) {
  piface = getPiface(project)
  if (is.null(piface)) {
    return(NULL)
  }
  outputs = piface$pipelines$pepatac.py$outputs
  outputs
}

#' Populates and returns output files
#' @param project pepr::Project object
#' @export
getOutFiles = function(project) {
    outputs = getOutputs(project)
    if (is.null(outputs)) {
        return(NULL)
    }
    prefix = ifelse(is.null(config(project)$metadata$results_subdir),
                  "results_pipeline", config(project)$metadata$results_subdir)
    prefix = file.path(prefix, "{sample$sample_name}/")
    prepend = function(path, prefix) {
        file.path(prefix, path)
    }
    outFilesFull = lapply(outputs, prepend, prefix)
    outFilesPopulated = lapply(outFilesFull, populateString, project)
    return(outFilesPopulated)
}

