#' Get outputs from pipeline
#' 
#' Extracts the output file templates defined for a given pipeline
#'
#' @param pipeline an object of \code{\link[pepr]{Config-class}} 
#'
#' @return named list of output path templates, 
#' like: \code{"aligned_{sample.genome}/{sample.sample_name}_sort.bam"}
.getOutputs = function(pipeline) {
    if (!pepr::checkSection(pipeline, OUTPUTS_SECTION)) {
        pipName = ifelse(is.null(pipeline$name), "provided", pipeline$name)
        warning("There is no '", OUTPUTS_SECTION , 
             "' section in the ", pipName," pipeline.")
        return(invisible(NULL))
    }
    pipeline[[OUTPUTS_SECTION]]
}

#' Populates and returns output files for a given protocol
#' 
#' Returns the pipeline outputs which are defined in the pipeline interface 
#' indicated in the \code{\link[pepr]{Project-class}}
#' 
#' @param project \code{\link[pepr]{Project-class}} object
#' @param ... other arguments passed to methods
#' 
#' @return a list of output file paths. The order of the first level of the 
#' list corresponds to the order of the pipeline interface files, second level 
#' order (named) reflects the pipelines within the files, the last level is a 
#' named list of file paths populated by the samples
#' 
#' @export
#' @examples 
#' projectConfig = system.file("extdata",
#' "example_peps-master",
#' "example_piface",
#' "project_config.yaml",
#' package = "BiocProject")
#' p = Project(file = projectConfig)
#' outputsByProtocols(p, "PROTO2")
setGeneric("outputsByProtocols", function(project, ...)
    standardGeneric("outputsByProtocols"), signature="project")

#' @describeIn outputsByProtocols extracts pipeline outputs for a given protocol or set of protocols
#' @param protocolNames char vector of protocol names to match the pipelines 
#' and return their outputs
setMethod("outputsByProtocols", c(project="Project"), function(project, protocolNames=NULL) {
    ret = list()
    # make sure no duplicates exist
    protocolNames = unique(protocolNames)
    pifaces = getPipelineInterfaces(project)
    for (i in seq_along(pifaces)) {
        pifaceRet = list()
        protoMappings = getProtocolMappings(pifaces[[i]])
        pifaceProtoNames = names(protoMappings)
        # find protocol names that both exist in the pipeline interface 
        # and were requested. If none are requested, return all the info held 
        # in all the pipeline interfaces
        if (is.null(protocolNames)) {
            validProtoNames = pifaceProtoNames
        } else {
            validProtoNames = pifaceProtoNames[match(protocolNames, pifaceProtoNames)]
            validProtoNames = validProtoNames[!is.na(validProtoNames)]
        }
        # if there are none, skip iteration
        if (length(validProtoNames) < 1) next
        protRet = list()
        for (j in seq_along(validProtoNames)) {
            # get the pipelines that match the protocol
            pipelines = getPipelines(pifaces[[i]], validProtoNames[j])
            pipRet = list()
            for (k in seq_along(pipelines)) {
                # get the output templates fot the pipeline
                pipelineOutputs = .getOutputs(pipelines[[k]])
                # if there are none, skip iteration
                if (is.null(pipelineOutputs)) next
                # populate the templates with the sample data, only samples that
                # have the protocol attribute matching the protocol are used
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
    }
    ret
})

#' Populates and returns output files for a given pipeline
#' 
#' Returns the pipeline outputs which are defined in the pipeline interface 
#' indicated in the \code{\link[pepr]{Project-class}}
#' 
#' Only the samples that have a matching protocol attribute will be used to
#' populate the output paths.
#' 
#' @note If there are multiple pipeline interfaces that defined the same 
#' pipeline (name), the output files of first one will be returned. 
#' \cr \cr If no pipeline name provided, output for all pipelines with unique 
#' names are returned
#' 
#' @param project \code{\link[pepr]{Project-class}} object
#' @param ... other arguments passed to methods
#' 
#' @return a named list of output file paths for the requested pipeline
#' 
#' @export
#' @examples 
#' projectConfig = system.file("extdata",
#' "example_peps-master",
#' "example_piface",
#' "project_config.yaml",
#' package = "BiocProject")
#' p = Project(file = projectConfig)
#' outputsByPipeline(p, "pipeline2.py")
setGeneric("outputsByPipeline", function(project, ...)
    standardGeneric("outputsByPipeline"), signature = "project")

#' @describeIn outputsByPipeline extracts pipeline outputs for a given pipeline
#' @param pipelineName pipeline name to return the outputs for
setMethod("outputsByPipeline", c(project="Project"), function(project, pipelineName=NULL) {
    if(!is.null(pipelineName) && length(pipelineName) != 1)
        stop("Only one pipeline can be specified, got ", length(pipelineName),
             ". To get outputs for all defined pipelines, skip the argument")
    allOutputs = outputsByProtocols(project)
    allPips = list()
    for (piface in allOutputs) {
        for (protocol in piface) {
            if (is.null(pipelineName)) {
                # if no pipelines requested, collect ouput info 
                allPips = append(allPips, protocol)
            } else {
                # if pipeline requested, check for name match 
                # and return outputs if so
                matchPips = match(pipelineName, names(protocol))
                if (!is.na(matchPips)) {
                    allPips = .unionList(allPips, protocol[matchPips][[1]], T)
                }
            }
        }
    }
    if (!is.null(pipelineName)) {
        if (length(allPips) < 1) {
            # if no pipelines matched by the requested name, warn 
            warning("No outputs match for the pipeline: ", pipelineName)
            return(invisible(NULL))
        } else {
            return(allPips)
        }
    } else {
        # return only the unique pipelines in terms of their names
        return(allPips[unique(names(allPips))])
    }
})

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
#' projectConfig = system.file("extdata",
#' "example_peps-master",
#' "example_piface",
#' "project_config.yaml",
#' package = "BiocProject")
#' p = Project(file = projectConfig)
#' pifaces = getPipelineInterfaces(p)
#' getPipelines(pifaces[[1]])
setGeneric("getPipelines", function(.Object, protocolName=NULL)
    standardGeneric("getPipelines"))

#' @describeIn getPipelines extracts pipelines from a pipeline interface
setMethod("getPipelines", "Config",function(.Object, protocolName){
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
            faultyProtoNames = which(is.na(names(pipelineNames)))
            if (length(faultyProtoNames) > 0) {
                warning(paste0(protocolName[faultyProtoNames], collapse = ", "), 
                        " did not match any known protocols")
            }
            idx = match(unlist(pipelineNames), names(.Object[[PIPELINES_SECTION]]))
            # account for faulty protocol names
            misMatches = which(is.na(idx))
            if (length(misMatches) > 0) {
                idx = idx[-misMatches]
            }
            if (length(idx) < 1) {
                warning("No pipelines were matched by: ", 
                        paste0(protocolName, collapse = ", "))
                return(invisible(NULL))
            }
            selectedPipelines = .Object[[PIPELINES_SECTION]][idx]
            lapply(selectedPipelines, function(x) {
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
#' projectConfig = system.file("extdata",
#' "example_peps-master",
#' "example_piface",
#' "project_config.yaml",
#' package = "BiocProject")
#' p = Project(file = projectConfig)
#' pifaces = getPipelineInterfaces(p)
#' getProtocolMappings(pifaces[[1]])
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
    cfgMetadata = config(project)$metadata
    prefix = ifelse(is.null(cfgMetadata$results_subdir),
                    file.path(cfgMetadata$output_dir,"results_pipeline"),
                    cfgMetadata$results_subdir)
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
#' projectConfig = system.file("extdata",
#' "example_peps-master",
#' "example_piface",
#' "project_config.yaml",
#' package = "BiocProject")
#' p = Project(file = projectConfig)
#' samplesByProtocol(samples(p), "PROTO2")
samplesByProtocol = function(s, protocolName, caseSensitive=FALSE) {
    if (length(protocolName) != 1)
        stop("Select just one protocol name to select the subset of samples")
    if (!caseSensitive)
        s[,which(colnames(s) == "protocol")] = 
            tolower(s[,which(colnames(s) == "protocol")])
    
    ret = subset(s, protocol==tolower(protocolName))
    if (NROW(ret) == 0)
        warning(protocolName, " did not match any known protocols")
    return(ret)
}

#' Get the pipeline intraface(s)
#'
#' Extracts the pipeline interface(s) as a list 
#' of \code{\link{Config-class}} objects
#' 
#' @param .Object an object of \code{\link{Project-class}}
#'
#' @return a list of objects of \code{\link{Config-class}} 
#' (list-like representation of the YAML file(s)). Number of elements in the 
#' list reflects the number of pipeline interfaces defined by the 
#' \code{\link{Project-class}} object.
#' @export
#'
#' @examples
#' projectConfig = system.file("extdata",
#' "example_peps-master",
#' "example_piface",
#' "project_config.yaml",
#' package = "BiocProject")
#' p = Project(file = projectConfig)
#' getPipelineInterfaces(p)
setGeneric("getPipelineInterfaces", function(.Object)
    standardGeneric("getPipelineInterfaces"))

#' @describeIn getPipelineInterfaces extracts pipeline interfaces defined in a \code{\link{Project-class}} object
setMethod("getPipelineInterfaces", "Project", function(.Object) {
    if(.hasPipIface(.Object)){
        cfg = config(.Object)
        for(sect in PIP_IFACE_SECTION){
            cfg = cfg[[sect]]
        }
        lapply(as.list(cfg), function(x) {
            methods::new("Config", yaml::yaml.load_file(x))
        })
    } else{
        warning("No pipeline interface found in the config")
        invisible(NULL)
    }
})

#' Check if project defines pipeline interface
#'
#' @param p object of \code{\link{Project-class}}
#'
#' @return logical indicating whether pipeline interface is defined
.hasPipIface = function(p) {
    checkSection(config(p), PIP_IFACE_SECTION)
}

