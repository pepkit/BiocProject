#' @export
setMethod(
  f = "initialize",
  signature = "BiocProject",
  definition = function(.Object, file, subproject) {
    .Object = loadPEP(.Object, file, subproject)
    .Object
  }
)

#' Load the PEP into the \code{BiocProject}
#'
#' This function loads the PEP into the metadata slot of the \code{\link{BiocProject}} object using the \code{\link[pepr]{Project}} constructor of the pepr package
#'
#' This funciton is used in the \code{\link{BiocProject}} initialize method and should not be used outside
#'
#' @param .Object object of class \code{\link{BiocProject}}
#' @param file a character vecotr with a path to the config file
#' @param subproject a character vector with a name of the subproject to be activated
#'
#' @seealso \url{https://pepkit.github.io/}
#'
#' @return object of class \code{\link{BiocProject}} with the metadata slot populated with the Project object
#'
setGeneric("loadPEP", function(.Object, file, subproject)
  standardGeneric("loadPEP"))

setMethod(
  f = "loadPEP",
  signature = "BiocProject",
  definition = function(.Object, file, subproject) {
    if (!identical(file, character(0))) {
      .Object@metadata = list(PEP = pepr::Project(file = file, subproject = subproject))
    } else{
      message("\nNo config file provided. Creating empty BiocProject object metadata...\n")
    }
    return(.Object)
  }
)

#' @export
setMethod(
  f = "samples",
  signature = "Annotated",
  definition = function(object) {
    if (!is.null(object@metadata$PEP) &
        methods::is(object@metadata$PEP, "Project")) {
      pepr::samples(object = object@metadata$PEP)
    } else{
      return()
    }
  }
)


#' @export
setMethod(
  f = "config",
  signature = "Annotated",
  definition = function(object) {
    if (!is.null(object@metadata$PEP) &
        methods::is(object@metadata$PEP, "Project")) {
      ret = pepr::config(object = object@metadata$PEP)
      return(ret)
    } else{
      return()
    }
  }
)

#' @export
setMethod(
  f = "getSubsample",
  signature = signature(
    .Object = "Annotated",
    sampleName = "character",
    subsampleName = "character"
  ),
  definition = function(.Object, sampleName, subsampleName) {
    if (!is.null(.Object@metadata$PEP) &
        methods::is(object@metadata$PEP, "Project")) {
      ret = pepr::getSubsample(
        .Object = .Object@metadata$PEP,
        sampleName = sampleName,
        subsampleName = subsampleName
      )
      return(ret)
    } else{
      return()
    }
  }
)

#' Read any biological data to the BiocProject format
#'
#' This function allows for reading of any biological data into the \code{\link{BiocProject}} format based on the metadata enclosed in the \code{\link[pepr]{Project}} object.
#'
#' If the \code{func} parameter is set to \code{TRUE} then the function name that will be used to read the data in is taken from the config slot in \code{\link[pepr]{Project}} (specifically: \code{config(project)$bioconductor$parse_code}). \cr If the \code{func} is set to string then the function of this name will be used to read the data in.\cr If the \code{func} is set to \code{FALSE} then an \code{\link{BiocProject}} object with no data is created.
#'
#' The pepr::Project component of the returned object is constructed from the provided \code{file} argument with the path to the PEP config file (and \code{subproject}, optionally). Alternatively, the \code{\link[pepr]{Project}} can be provided as a \code{project} argument.
#' 
#' The \code{readBiocData} function will try to find the function in the environment or will source it from the file named as the \code{func} argument specifies (with the \code{.R} extension).
#' @param file a character containing a path to the PEP config file (see \url{https://pepkit.github.io/docs/project_config/} for more details)
#' @param subproject a character containing the name of the subproject that should be activated
#' @param project an object of class \code{\link[pepr]{Project}}
#' @param func a boolean or a name of the function to use. See Details for more information
#'
#' @return object of the class that is returned with the provided function or an empty \code{\link[pepr]{Project}} object if no function was provided (\code{func} parameter was \code{FALSE})
#' @export
#'
#' @examples
#' \dontrun{
#' projectConfig = system.file(
#' "extdata",
#' "example_peps-master",
#' "example_BiocProject",
#' "project_config.yaml",
#' package = "BiocProject"
#' )
#' source(system.file(
#' "extdata",
#' "example_peps-master",
#' "example_BiocProject",
#' "parseEncodeRegions.R",
#' package = "BiocProject"
#' ))
#' PEP = Project(projectConfig)
#' readBiocData(PEP,TRUE)
#' }
readBiocData <- function(file = character(), subproject = character(), func = FALSE, ...) {
  if(length(subproject) != 0 & length(file) != 0){
    project = suppressWarnings(pepr::Project(file = file,subproject = subproject))
  }else{
    if(!length(file) == 0){
      project = suppressWarnings(pepr::Project(file = file))
    }else{
      ellipsis = list(...)
      if (length(ellipsis$project) != 0){
        stopifnot(methods::is(ellipsis$project, "Project"))
      }else{
        stop("Neither the config file path nor the pepr::Project object was provided.")
      }
    }
  }
  
  if (methods::is(func, "logical")) {
    if (func) {
      funcName = pepr::config(project)$bioconductor$parse_code
    } else{
      message("No data was read. Returning a pepr::Project object...")
      return(project)
    }
  } else{
    if (methods::is(func, "character")) 
      funcName = func
  }
  if (!exists(funcName)) {
    if (!file.exists(paste0(funcName, ".R"))) {
      stop(
        "The function ",
        funcName,
        " does not exist. Read it in or move the file with that function to the CWD: ",
        getwd()
      )
      return(NULL)
    } else{
      source(paste0(funcName, ".R"))
    }
  }
  data = do.call(funcName, list(project))
  message("The function ", funcName, " was used to read the data in.")
  if (methods::is(data, "Annotated")) {
    data@metadata = list(PEP = project)
    return(data)
  } else{
    message(
      "The object read with the function: ",
      funcName,
      " (" ,
      class(data)[1],
      ") does not extend the class Annotated. The Project object could not be added to the metadata section."
    )
  }
}
