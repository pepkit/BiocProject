
#' Collect all pipeline interfaces
#' 
#' Collects all relevant pipeline interfaces 
#' for this \code{\link[pepr]{Project-class}}
#'
#' @param project \code{\link[pepr]{Project-class}} object
#' @param ... other arguments 
#'
#' @return a list of pipeline interface file paths.
#'
#' @export
#' @examples
#' projectConfig = system.file('extdata',
#' 'example_peps-master',
#' 'example_piface',
#' 'project_config.yaml',
#' package = 'BiocProject')
#' p = Project(file = projectConfig)
#' gatherPipelineInterfaces(p)
#' gatherPipelineInterfaces(p, TRUE)
setGeneric("gatherPipelineInterfaces", 
           function(project, ...) standardGeneric("gatherPipelineInterfaces"), 
           signature = "project")

#' @describeIn gatherPipelineInterfaces Collect all pipeline interfaces
#' @param projectLevel logical indicating whether a only project-level pifaces 
#' should be considered. Otherwise, only sample-level ones are.
#' @importFrom stats setNames
setMethod("gatherPipelineInterfaces", 
          c(project = "Project"), function(project, 
                                           projectLevel = FALSE) {
              if (!projectLevel) {
                  return(.gatherSamplePipelineInterfaces(project))
              } else {
                  pik = PIP_IFACE_NAME
                  if (!is.null(config(project)[[LOOPER_SECTION]][[PIP_IFACE_KEY]])) 
                      pik = config(project)[[LOOPER_SECTION]][[PIP_IFACE_KEY]]
                  if (!is.null(config(project)[[LOOPER_SECTION]][[pik]])) 
                      return(setNames(vapply(unlist(
                          config(project)[[LOOPER_SECTION]][[pik]]), 
                          function(x) {
                              pepr::.makeAbsPath(x, parent=dirname(project@file))
                          }, character(1)), 
                          NULL))
                  warning("No project pipeline interfaces defined")
                  return(invisible(NULL))
              }
          })


setGeneric(".gatherSamplePipelineInterfaces", 
           function(project) standardGeneric(".gatherSamplePipelineInterfaces"), 
           signature = "project")

#' @describeIn gatherPipelineInterfaces extracts pipeline outputs 
#' for a given pipeline
#' @importFrom pryr partial
setMethod(".gatherSamplePipelineInterfaces", 
          c(project = "Project"), function(project) {
              t = pepr::sampleTable(project)
              .mkAbs = pryr::partial(pepr::.makeAbsPath, parent=dirname(project@file))
              if (PIP_IFACE_NAME %in% colnames(t)) 
                  return(setNames(vapply(unique(unlist(t[, PIP_IFACE_NAME])), 
                                         .mkAbs, character(1)), NULL))
              return(invisible(NULL))
          })


#' Get pipeline interfaces by sample
#' 
#' Collects all relevant pipeline interfaces for this 
#' \code{\link[pepr]{Project-class}} and provides a sample to interfaces mapping
#'
#' @param project \code{\link[pepr]{Project-class}} object
#'
#' @return a list of pipeline interface file paths keyed by sample names
#'
#' @export
#' @examples
#' projectConfig = system.file('extdata',
#' 'example_peps-master',
#' 'example_piface',
#' 'project_config.yaml',
#' package = 'BiocProject')
#' p = Project(file = projectConfig)
#' pipelineInterfacesBySample(p)
setGeneric("pipelineInterfacesBySample", 
           function(project) standardGeneric("pipelineInterfacesBySample"), 
           signature = "project")

#' @describeIn pipelineInterfacesBySample Get pipeline interfaces by sample
setMethod("pipelineInterfacesBySample", 
          c(project = "Project"), function(project) {
              t = pepr::sampleTable(project)
              if (PIP_IFACE_NAME %in% 
                  colnames(t)) {
                  .mkAbs = pryr::partial(pepr::.makeAbsPath, 
                                         parent=dirname(project@file))
                  pifaces = t[, PIP_IFACE_NAME]
                  names(pifaces) = unlist(t[, 
                                            "sample_name"])
                  return(lapply(pifaces, .mkAbs))
              }
              return(invisible(NULL))
          })

#' Populates and returns output files for a given sample
#'
#' Returns the sample level pipeline outputs which are defined in the output 
#' schema indicated by the pipeline interface indicated in the 
#' \code{\link[pepr]{Project-class}}
#'
#' @param project \code{\link[pepr]{Project-class}} object
#' @param ... other arguments
#' 
#' @return a list of output file paths. The order of the first level of the
#' list corresponds to the order of the pipeline interface files, second level 
#' is a named list of file paths populated by the samples
#'
#' @export
#' @examples
#' projectConfig = system.file('extdata',
#' 'example_peps-master',
#' 'example_piface',
#' 'project_config.yaml',
#' package = 'BiocProject')
#' p = Project(file = projectConfig)
#' getOutputsBySample(p)
#' getOutputsBySample(p, 'sample1')
setGeneric("getOutputsBySample", 
           function(project, ...) standardGeneric("getOutputsBySample"), 
           signature = "project")

#' @describeIn getOutputsBySample Populates and returns output files
#'  for a given sample
#' @param sampleNames names of the samples 
#' @importFrom yaml yaml.load_file
setMethod("getOutputsBySample", 
          c(project = "Project"), function(project, 
                                           sampleNames = NULL) {
              pifacesBySample = pipelineInterfacesBySample(project = project)
              defSampleNames = names(pifacesBySample)
              if (!is.null(sampleNames)) 
                  defSampleNames = intersect(sampleNames, defSampleNames)
              if (length(defSampleNames) < 1) 
                  stop("No samples matched by: ", 
                       paste0(sampleNames, collapse = ","))
              ret = list()
              for (sampleName in defSampleNames) {
                  sampleRet = list()
                  pifaceSources = pifacesBySample[[sampleName]]
                  for (pifaceSource in pifaceSources) {
                      piface = yaml::yaml.load_file(pifaceSource)
                      if (!.checkPifaceType(piface, "sample")) 
                          return(invisible(NULL))
                      if(!OUTPUT_SCHEMA_SECTION %in% names(piface)) next
                      schema = readSchema(
                          piface[[OUTPUT_SCHEMA_SECTION]], dirname(pifaceSource))
                      sampleRet[[piface[[PIP_NAME_KEY]]]] = .populateSchemaPaths(
                          schema=schema, project=project, sampleName=sampleName)
                  }
                  ret[[sampleName]] = sampleRet
              }
              ret
          })

#' Populates and returns outputs for a given \code{\link[pepr]{Project-class}}
#'
#' Returns the project level pipeline outputs which are defined in the output 
#' schema indicated by the pipeline interface indicated in the 
#' \code{\link[pepr]{Project-class}}
#'
#' @param project \code{\link[pepr]{Project-class}} object
#'
#' @return a list of output file paths. The order of the first level of the
#' list corresponds to the order of the pipeline interface files, second level 
#' is a named list of file paths populated 
#' by the \code{\link[pepr]{Project-class}}
#'
#' @export
#' @examples
#' projectConfig = system.file('extdata',
#' 'example_peps-master',
#' 'example_piface',
#' 'project_config.yaml',
#' package = 'BiocProject')
#' p = Project(file = projectConfig)
#' getProjectOutputs(p)
setGeneric("getProjectOutputs", 
           function(project) standardGeneric("getProjectOutputs"), 
           signature = "project")

#' @describeIn getProjectOutputs Populates and returns output files for 
#' a given \code{\link[pepr]{Project-class}}
setMethod("getProjectOutputs", 
          c(project = "Project"), function(project) {
              pifaceSources = gatherPipelineInterfaces(
                  project, projectLevel=TRUE)
              ret = list()
              for (pifaceSource in pifaceSources) {
                  piface = yaml::yaml.load_file(pifaceSource)
                  if (!.checkPifaceType(piface, "project")) 
                      return(invisible(NULL))
                  if(!OUTPUT_SCHEMA_SECTION %in% names(piface)) next
                  schema = readSchema(
                      piface[[OUTPUT_SCHEMA_SECTION]], dirname(pifaceSource))
                  ret[[piface[[PIP_NAME_KEY]]]] = .populateSchemaPaths(
                      schema, project, NULL, projectContext=TRUE)
              }
              ret
          })

#' Populate values in output schema
#' 
#' Populates schema values of type path and thumbnail path in the provided 
#' output schema for the selected sample or project
#'
#' @param schema schema with value templates to populate 
#' @param project \code{\link[pepr]{Project-class}} object
#' @param projectContext whether the values for path templates populating 
#' should be sourced from the project metadata. Otherwise metadata for 
#' a selected sample is used
#' @param sampleName name of the sample to populate the outputs for. Required 
#' if \code{projectContext} set to \code{FALSE}
#'
#' @return a possibly nested list of length equal to the number of results defined in 
#' the schema with populated outputs
.populateSchemaPaths <- function(
    schema, project, projectContext=FALSE, sampleName=NULL) {
    ret = list()
    if(!projectContext && is.null(sampleName))
        stop("Must specify sample to populate schema path templates for in no 
             project context mode")
    if(projectContext) sampleName = NULL
    for(i in seq_along(schema)){
        if("value" %in% names(schema[[i]])) {
            if(is(schema[[i]][["value"]], "list")) {
                ret[[i]] = .populateRecursively(
                    schema[[i]][["value"]], project, sampleName, projectContext)
            } else {
                ret[[i]] = schema[[i]][["value"]]
            }
        }            
    }
    return(ret)
}


#' Recursively populate paths in results of type object
#'
#' @param l list to populate paths in
#' @param project \code{\link[pepr]{Project-class}} object
#' @param sampleName name of the sample to populate the outputs for
#'
#' @return list with populate paths
.populateRecursively <- function(l, project, sampleName, projectContext=FALSE) {
    namesL = names(l)
    if(projectContext) sampleName = NULL
    for(i in seq_along(l)) {
        if(is(l[[i]], "list")){
            l[[i]] = .populateRecursively(
                l[[i]], project, sampleName, projectContext)
        } else{
            if(namesL[i] == "path" || namesL[i] == "thumbnail_path") 
                l[[i]] = .populateString(
                    string=l[[i]], project=project, sampleName=sampleName, 
                    projectContext=projectContext)
        }
    }
    return(l)
}





