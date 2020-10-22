
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
