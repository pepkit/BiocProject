#' Switches from python to R list accession syntax
#' 
#' Python uses a dot to access attributes, while R uses \code{$}; this function
#' converts the python style into R so that we can use R code to populate
#' variables with R lists. From this: '\code{sample.name}' to this: '\code{sample$name}'
#' @param str String to recode
#' @return string with the recoded accession syntax
#' @examples
#' pyToR("{sample.name}...{project.source.name}")
#' @export
pyToR = function(str) {
    # This is the regex where the magic happens
    pytor = function(str) gsub("(\\{[^\\.\\}]+)\\.", "\\1$", str)
    # This loop allows multi-layer accession
    res = str
    prev = ""
    while (prev != res) {
        prev = res
        res = pytor(res)
    }
    return(res)
}

#' Populates a variable-encoded string with sample/project variables
#' 
#' Given a string and a project this function will go through samples and populate
#' the variables. Used to return real files for each sample from an output variable
#' in the pipeline interface
#' 
#' @param string Variable-encoded string to populate
#' @param \code{\link[pepr]{Project-class}} object with values to draw from
#' 
#' @return populated string
#' @importMethodsFrom pepr samples
#' @importMethodsFrom glue glue
#' 
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
        with(list(sample=s, project=project), glue::glue(pyToR(string)))
    })
    return(populatedStrings)
}


#' Get the outputs
#'
#' Gets the pipeline outputs which are defined in the pipeline interface indicated in the \code{\link[pepr]{Project-class}}
#'
#' @param project an object of \code{\link[pepr]{Project-class}}
#'
#' @return named list of output path templates, like: \code{"aligned_{sample.genome}/{sample.sample_name}_sort.bam"}
#' @export
#' @importMethodsFrom pepr getPipelineInterface
#' @examples
#' #test
getOutputs = function(project) {
  piface = pepr::getPipelineInterface(project)
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

