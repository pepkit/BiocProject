#' Switches from python to R list accession syntax
#' 
#' Python uses a dot to access attributes, while R uses \code{$}; this function
#' converts the python style into R so that we can use R code to populate
#' variables with R lists. From this: '\code{sample.name}' to this: '\code{sample$name}'
#' @param str String to recode
#' @return string with the recoded accession syntax
#' @examples
#' pyToR("{sample.name}...{project.source.name}")
.pyToR = function(str) {
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
#' @importFrom glue glue
#' 
#' @examples
#' configFile = system.file(
#'   "extdata",
#'   "example_peps-master",
#'   "example_BiocProject",
#'   "project_config.yaml",
#'   package = "BiocProject"
#' )
#' bp = BiocProject(file=configFile)
#' .populateString("{sample.sample_name}/{sample.file_path}", bp)
.populateString = function(string, project) {
    # Apply this glue function on each row in the samples table,
    # coerced to a list object to allow attribute accession.
    populatedStrings = apply(pepr::samples(project), 1, function(s) {
        with(list(sample=s, project=project), glue::glue(.pyToR(string)))
    })
    return(populatedStrings)
}


#' Get outputs from pipeline
#' 
#' Extracts the output file templates defined for a given pipeline
#'
#' @param .Object pipeline, an object of \code{\link[pepr]{Config-class}} 
#'
#' @return named list of output path templates, like: \code{"aligned_{sample.genome}/{sample.sample_name}_sort.bam"}
#' @export
#'
#' @examples
#' # add example
setGeneric(".getOutputs", function(.Object)
    standardGeneric(".getOutputs"))

#' @describeIn .getOutputs extracts output templates from a pipeline
setMethod(".getOutputs","Config",function(.Object){
    if(!pepr::checkSection(.Object, OUTPUTS_SECTION)){
        pipName = ifelse(is.null(.Object$name),"provided",.Object$name)
        warning("There's no '", OUTPUTS_SECTION , 
             "' section in the ", pipName," pipeline.")
        return(invisible(NULL))
    }
    .Object[[OUTPUTS_SECTION]]
})

#' Populates and returns output files
#' 
#' Returns the pipeline outputs which are defined in the pipeline interface indicated in the \code{\link[pepr]{Project-class}}
#' 
#' @param project \code{\link[pepr]{Project-class}} object
#' 
#' @return a list of output file paths
#' 
#' @export
#' @examples 
#' #add examples
getOutFiles = function(project, pipelineNames=NULL, protocolNames=NULL) {
    if(!is.null(pipelineNames) && !is.null(protocolNames))
        stop("Use just one pipeline outputs selection method")
    outputs = NULL
    pifaces = getPipelineInterfaces(project)
    # message(length(pifaces), " pipeline intrafaces found")
    cnt = 0
    ret = list()
    for (piface in pifaces) {
        pipN = pipelineNames
        proN = protocolNames
        cnt = cnt + 1
        pipelines = getPipelines(piface)
        # message("pipeline interface ", cnt, " defines: ", paste0(names(pipelines), collapse=", "))
        protoMappings = getProtocolMappings(piface)
        protocolMappingNames = names(protoMappings)
        if (!all(proN %in% protocolMappingNames)){
            warning("Not all protocolNames are vaild protocols in the pipeline ",
                 "interface ", cnt,". Select from: ", paste0(protocolMappingNames,
                                                   collapse=", "))
            next
        }
        idx = match(proN, protocolMappingNames)
        idx = idx[!is.na(idx)]
        if (length(idx) > 0) {
            if(all(is.na(match(names(pipelines), protoMappings[[idx]]))))
                warning("No pipelines in pipeline_interface ", cnt," were matched by selected protocol: ", proN)
            pipN = append(pipN, protoMappings[[idx]])
            message("protocolNames: ", paste0(proN, collapse=", "),
                    " were used for pipelineNames selection: ",
                    paste0(pipN, collapse=", "))
        }
        idx = which(names(pipelines) == pipN)
        if (length(idx) > 0) {
            pipelines = pipelines[idx]
            pipOutputs = list()
            for(i in seq_along(pipelines)){
                pipelineOutputs = .getOutputs(pipelines[[i]])
                if(is.null(pipelineOutputs)) next
                pipOutputs[[names(pipelines)[i]]] = .populateTemplates(project, pipelineOutputs)
            }
            ret[[cnt]] = pipOutputs
        }
    }
    if (length(ret) < 1){
        stop("No pipelines matched the requirements")
        return(invisible(NULL))
    }
    ret
}

#' Get pipelines defined within a pipeline interface
#'
#' @param .Object a pipeline interface, an object of \code{\link[pepr]{Config-class}} 
#'
#' @return a list of pipelines, objets of \code{\link[pepr]{Config-class}} 
#' @export
#'
#' @examples
#' # add example
setGeneric("getPipelines", function(.Object)
    standardGeneric("getPipelines"))

#' @describeIn getPipelines extracts pipelines from a pipeline interface
setMethod("getPipelines","Config",function(.Object){
    if(checkSection(.Object, PIPELINES_SECTION)){
        lapply(.Object[[PIPELINES_SECTION]], function(x){
            methods::new("Config", x)
        })
    }else{
        warning("The '", PIPELINES_SECTION
                ,"' section is not defined in the provided pipeline interface.")
        invisible(NULL)
    }
})

#' Get protocol mappings defined within a pipeline interface
#'
#' @param .Object a pipeline interface, an object of \code{\link[pepr]{Config-class}} 
#'
#' @return a named list, where the names of the elements are the 
#' protocols and the elements names of the pipelines
#' @export
#'
#' @examples
#' # add example
setGeneric("getProtocolMappings", function(.Object)
    standardGeneric("getProtocolMappings"))

#' @describeIn getProtocolMappings extracts protocol mappings from a pipeline interface
setMethod("getProtocolMappings","Config",function(.Object){
    if(checkSection(.Object, PROTO_MAP_SECTION)){
        lapply(.Object[[PROTO_MAP_SECTION]], function(x){
            x
        })
    }else{
        warning("The '", PROTO_MAP_SECTION
                ,"' section is not defined in the provided pipeline interface.")
        invisible(NULL)
    }
})

#' Populate list of path templates
#'
#' @param project an object of \code{\link[pepr]{Config-class}} 
#' @param templList list of strings, like: "aligned_{sample.genome}/{sample.sample_name}_sort.bam"
#'
#' @return list of strings
.populateTemplates <- function(project, templList) {
    prefix = ifelse(is.null(config(project)$metadata$results_subdir),
                    "results_pipeline", config(project)$metadata$results_subdir)
    prefix = file.path(prefix, "{sample$sample_name}/")
    prepend = function(path, prefix) {
        file.path(prefix, path)
    }
    outFilesFull = lapply(templList, prepend, prefix)
    lapply(outFilesFull, .populateString, project)
}
