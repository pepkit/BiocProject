#' @export
setMethod(
  f = "initialize",
  signature = "BiocProject",
  definition = function(.Object,file, ...){
    .Object = loadPEP(.Object,file)
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
#' @param file a string with a path to the config file
#' 
#' @seealso \url{https://pepkit.github.io/} 
#'
#' @return object of class \code{\link{BiocProject}} with the metadata slot populated with the Project object
#'
setGeneric("loadPEP", function(.Object, file, ...)
  standardGeneric("loadPEP"))

setMethod(
  f = "loadPEP",
  signature = "BiocProject",
  definition = function(.Object, file, ...) {
    if (!identical(file, character(0))) {
      .Object@metadata = list(PEP = pepr::Project(file = file))
    } else{
      message("No config file provided. Creating empty BiocProject object metadata...")
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
      ret=pepr::samples(object = object@metadata$PEP)
      invisible(ret)
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
    if (methods::is(object, "BiocProject")) {
      ret=pepr::config(object = object@metadata$PEP)
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
      ret=pepr::getSubsample(
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
  f="activateSubproject",
  signature = signature(.Object="Annotated",sp="character"),
  definition = function(.Object,sp){
    if (methods::is(.Object, "BiocProject")) {
      ret=pepr::activateSubproject(.Object = .Object@metadata$PEP, sp=sp)
      return(ret)
    } else{
      return()
    }
  }
)


