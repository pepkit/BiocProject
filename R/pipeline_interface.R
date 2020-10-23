
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


#' Get outputs from pipeline defined in an output schema
#' 
#' Extracts the output file templates defined for a given pipeline
#'
#' @param pipeline an object of \code{\link[pepr]{Config-class}} 
#' @param parent a path to parent folder to use
#' @param projectContext logical indicating whether a only project-level 
#' pifaces should be considered. Otherwise, only sample-level ones are. 
#'
#' @return named list of output path templates, 
#' like: \code{'aligned_{sample.genome}/{sample.sample_name}_sort.bam'}
.getOutputs = function(pipeline, parent, projectContext = FALSE) {
    if (!OUTPUT_SCHEMA_SECTION %in% 
        names(pipeline)) 
        return(invisible(NULL))
    outputSchema = readSchema(pipeline[[OUTPUT_SCHEMA_SECTION]], parent)
    sect = "properties"
    if (!projectContext) 
        sect = SCHEMA_SAMPLE_OUTS
    if (!pepr::.checkSection(outputSchema, sect)) {
        pipName = ifelse(is.null(pipeline[[PIP_NAME_KEY]]), 
                         "provided", pipeline[[PIP_NAME_KEY]])
        warning("There is no '", 
                paste(sect, collapse = ":"), 
                "' section in the ", 
                pipName, " pipeline output schema.")
        return(invisible(NULL))
    }
    outputs = outputSchema[[sect]]
    if ("samples" %in% names(outputs)) 
        outputs[["samples"]] = NULL
    x = lapply(outputs, function(x) {
        return(x[["path"]])
    })
    if (is.null(unlist(lapply(x, is.null)))) 
        return(invisible(NULL))
    return(x)
}

#' Populates and returns output files for a given sample
#'
#' Returns the pipeline outputs which are defined in the pipeline interface
#' indicated in the \code{\link[pepr]{Project-class}}
#'
#' @param project \code{\link[pepr]{Project-class}} object
#' @param ... other arguemnts
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
                      outputs = .getOutputs(piface, parent = dirname(pifaceSource))
                      sampleRet[[piface[[PIP_NAME_KEY]]]] = 
                          .populateTemplates(project, outputs, sampleName)
                  }
                  ret[[sampleName]] = sampleRet
              }
              ret
          })

#' Populates and returns output files for a
#'  given \code{\link[pepr]{Project-class}}
#'
#' Returns the pipeline outputs which are defined in the pipeline interface
#' indicated in the \code{\link[pepr]{Project-class}}
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
                  outputs = .getOutputs(piface, 
                                        parent = dirname(pifaceSource), 
                                        projectContext=TRUE)
                  ret[[piface[[PIP_NAME_KEY]]]] = 
                      .populateTemplates(project, outputs, projectContext=TRUE)
              }
              ret
          })
