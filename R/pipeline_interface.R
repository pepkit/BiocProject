#' Switches from python to R list accession syntax
#' 
#' Python uses a dot to access attributes, while R uses \code{$}; this function
#' converts the python style into R so that we can use R code to populate
#' variables with R lists. From this: '\code{sample.name}' to this: '\code{sample$name}'
#' @param str String to recode
#' @return string with the recoded accession syntax
#' @examples
#' .pyToR("{sample.name}...{project.source.name}")
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
#' @param protocolName string, name of the protocol to select the samples
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
.populateString = function(string, project, protocolName) {
    # Apply this glue function on each row in the samples table,
    # coerced to a list object to allow attribute accession.
    populatedStrings = apply(samplesByProtocol(samples(project), protocolName), 1, function(s) {
        with(list(sample=s, project=project), glue(.pyToR(string)))
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
        warning("There is no '", OUTPUTS_SECTION , 
             "' section in the ", pipName," pipeline.")
        return(invisible(NULL))
    }
    .Object[[OUTPUTS_SECTION]]
})

#' Populates and returns output files
#' 
#' Returns the pipeline outputs which are defined in the pipeline interface 
#' indicated in the \code{\link[pepr]{Project-class}}
#' 
#' @param project \code{\link[pepr]{Project-class}} object
#' @param pipelineNames char vector of pipeline names to return the outputs for
#' @param protocolNames char vector of protocol names to match the pipelines 
#' and return their outputs
#' 
#' @return a list of output file paths. The order of the first level of the 
#' list corresponds to the order of the pipeline interface files, second level 
#' order (named) reflects the pipelines within the files.
#' 
#' @export
#' @examples 
#' #add examples
getOutFiles = function(project, protocolNames=NULL) {
    # if(!is.null(pipelineNames) && !is.null(protocolNames))
        # stop("Use just one pipeline outputs selection method")
    ret = list()
    pifaces = getPipelineInterfaces(project)
    for (i in seq_along(pifaces)) {
        pifaceRet = list()
        protoMappings = getProtocolMappings(pifaces[[i]])
        pifaceProtoNames = names(protoMappings)
        validProtoNames = pifaceProtoNames[match(protocolNames, pifaceProtoNames)]
        validProtoNames = validProtoNames[!is.na(validProtoNames)]
        protRet = list()
        for (j in seq_along(validProtoNames)) {
            pipelines = getPipelines(pifaces[[i]], validProtoNames[j])
            pipRet = list()
            for (k in seq_along(pipelines)) {
                pipelineOutputs = .getOutputs(pipelines[[k]])
                if (is.null(pipelineOutputs)) next
                pipRet[[names(pipelines)[k]]] = 
                    .populateTemplates(project, pipelineOutputs, validProtoNames[j])
            }
            pifaceRet[[j]] = pipRet
        }
        names(pifaceRet) = validProtoNames
        ret[[i]] = pifaceRet
    }
    if (length(ret) < 1){
        stop("No pipelines matched the requirements")
        return(invisible(NULL))
    }
    ret
}

#' Get pipelines by protocol name
#' 
#' Gets the pipelines defined within the pipeline interface
#' 
#' To get all the pipelines (default) do not specify any \code{protocolName}
#'
#' @param .Object a pipeline interface, an object of \code{\link[pepr]{Config-class}} 
#' @param protocolName a string or vector of strings indicating the protocols 
#' for which the pipelines should be returned
#'
#' @return a list of pipelines, objets of \code{\link[pepr]{Config-class}} 
#' @export
#'
#' @examples
#' # add example
setGeneric("getPipelines", function(.Object, protocolName=NULL)
    standardGeneric("getPipelines"))

#' @describeIn getPipelines extracts pipelines from a pipeline interface
setMethod("getPipelines","Config",function(.Object, protocolName){
    if(checkSection(.Object, PIPELINES_SECTION)){
        # if PIPELINES_SECTION sectio found, proceed
        if (is.null(protocolName)){
            # if no protocolName provided, return all pipelines defined 
            # in the pipeline interface
            lapply(.Object[[PIPELINES_SECTION]], function(x){
                methods::new("Config", x)
            })
        } else {
            # if a specifc protocol name is requested, find the pipelines 
            # that match to that protocol and return just these
            pipelineNames = getProtocolMappings(.Object)[protocolName]
            idx = match(unlist(pipelineNames), names(.Object[[PIPELINES_SECTION]]))
            # account for faulty protocol names
            misMatches = which(is.na(idx))
            if (length(misMatches) > 0) {
                warning(paste0(protocolName[misMatches], collapse = ", "), 
                        " did not match any known protocols")
                idx = idx[-misMatches]
            }
            if (length(idx) < 1) {
                warning("No pipelines were matched by: ", 
                        paste0(protocolName, collapse = ", "))
                return(invisible(NULL))
            }
            selectedPipelines = .Object[[PIPELINES_SECTION]][idx]
            lapply(selectedPipelines, function(x){
                methods::new("Config", x)
            })
        }
    }else{
        # if no PIPELINES_SECTION section found, return null and warn
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
#' @param protocolName string, name of the protocol to select the samples
#'
#' @return list of strings
.populateTemplates = function(project, templList, protocolName) {
    prefix = ifelse(is.null(config(project)$metadata$results_subdir),
                    "results_pipeline", config(project)$metadata$results_subdir)
    prefix = file.path(prefix, "{sample$sample_name}/")
    prepend = function(path, prefix) {
        file.path(prefix, path)
    }
    outFilesFull = lapply(templList, prepend, prefix)
    lapply(outFilesFull, .populateString, project, protocolName)
}

#' Get samples that match the protocol 
#'
#' @param s samples, e.g. output of samples(project)
#' @param protocolName, string name of the protocol
#' @param caseSensitive, logical indicatinh whether the protocol match should be case sensitive
#'
#' @return a subset of samples that match the protocol
#' @export
#'
#' @examples
#' #add examples
samplesByProtocol = function(s, protocolName, caseSensitive=FALSE){
    if(!caseSensitive)
        s[,which(colnames(s) == "protocol")] = 
            tolower(s[,which(colnames(s) == "protocol")])
    subset(s, protocol==tolower(protocolName))
}
