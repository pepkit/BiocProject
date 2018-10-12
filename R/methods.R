#' @export
setMethod(
  f = "initialize",
  signature = "BiocProject",
  definition = function(.Object, file, subproject, ...) {
    .Object = loadPEP(.Object, file, subproject)
    return(.Object)
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
setGeneric("loadPEP", function(.Object, file, subproject, ...)
  standardGeneric("loadPEP"))

setMethod(
  f = "loadPEP",
  signature = "BiocProject",
  definition = function(.Object, file, subproject, ...) {
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
    if (methods::is(object, "BiocProject")) {
      ret = pepr::samples(object = object@metadata$PEP)
      invisible(ret)
    } else{
      message("TEST")
      return()
    }
  }
)

#' @export
setMethod(
  f = "config",
  signature = "Annotated",
  definition = function(object) {
    if (methods::is(object, "BiocProject")) {
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
    if (methods::is(.Object, "BiocProject")) {
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

#' @export
setMethod(
  f = "activateSubproject",
  signature = signature(.Object = "Annotated", sp = "character"),
  definition = function(.Object, sp) {
    if (methods::is(.Object, "BiocProject")) {
      ret = pepr::activateSubproject(.Object = .Object@metadata$PEP, sp = sp)
      return(ret)
    } else{
      return()
    }
  }
)


#' Read any biological data to the BiocProject object
#' 
#' This function allows for reading of any biological data into the \code{\link{BiocProject}} object based on the metadata enclosed in the \code{\link[pepr]{Project}} object. 
#' 
#' @param project an object of class \code{\link[pepr]{Project}}
#' @param func a boolean or a name of the function to use. See Details for more information
#'
#' @details If the \code{func} parameter is set to \code{TRUE} then the function name that will be used to read the data in is taken from the config slot in \code{\link[pepr]{Project}} (specifically: \code{config(project)$bioconductor$parse_code}). \cr If the \code{func} is set to string then the function of this name from the environment will be used to read the data in.\cr If the \code{func} is set to \code{FALSE} then an \code{\link{BiocProject}} object with no data is created. 
#'
#' @return object of the class that is returned with the provided function or an empty \code{\link{BiocProject}} object if no function was provided
#' @export
#'
#' @examples
readBiocData <- function(project, func = FALSE) {
  if (!is(project, "Project"))
    stop("The project argument is not a pepr::Project object.")
  if (is(func, "logical")) {
    if (func) {
      funcName = config(project)$bioconductor$parse_code
    } else{
      message("No data was read. Creating an empty BiocProject object...")
      return(BiocProject(file = project@file))
    }
  }
  if (is(func, "character")) {
    funcName = func
  }
  if (!is.null(funcName)) {
    if (exists(funcName)) {
      message("The function ", funcName, " will be used to read the data in.")
      data = do.call(funcName,list(project))
      if (is(data,"Annotated")){
        data@metadata = list(PEP = project)
      }else{
        message("The object read with the function", funcName, " (" ,class(data)[1],") does not extend the class Annotated. The Project could not be added to the metadata.")
      }
      return(data)
    } else{
      # Should this function try to look for it in the CWD? (source(paste0(funcName,".R")))
      message("The function ", funcName, " does not exist. Read in it.") 
    }
  }
}

