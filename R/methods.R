

#' @export
setMethod(
  f = "initialize",
  signature = "BiocProject",
  definition = function(.Object, func = FALSE, ...) {
    .Object = do.call(selectMethod("initialize",signature = "Project"),list(.Object, ...))
    if (methods::is(func, "logical")) {
      if (func) {
        funcName = pepr::config(.Object)$bioconductor$parse_code
      } else{
        message("No data was read. Creating an empty BiocProject object...")
        return(.Object)
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
    data = tryCatch({
      do.call(funcName, list(.Object))
    }, warning = function(w) {
      message("There are warnings associated with the ",funcName," execution")
    }, error = function(e) {
      stop("There are errors associated with the ",funcName," execution")
    }, finally = {
      message("The function ", funcName, " was used to read the data in.")
    })
    .Object[[length(.Object)+1]] = data
    return(.Object)
  }
)    

setMethod(
  f = "show",
  signature = "BiocProject",
  definition = function(object) {
    cat("BiocProject object. Class: ", class(object), fill = T)
    cat("  length: ", length(object), fill = T)
  })

#' This method coerces the \code{\link{BiocProject-class}} to \code{\link{Project-class}}
#'
#' @param .Object An object of \code{\link{BiocProject-class}}
#'
#' @return an object of \code{\link{Project-class}} object
#' 
#' @examples
#' ProjectConfig = system.file(
#' "extdata",
#' "example_peps-master",
#' "example_BiocProject",
#' "project_config.yaml",
#' package = "BiocProject"
#' )
#' 
#' bp = BiocProject(file=ProjectConfig)
#' toProject(bp)
#' 
#' @export
setGeneric("toProject", function(.Object, ...)
  standardGeneric("toProject"))

#' @export
setMethod(
  f = "toProject",
  signature = "BiocProject",
  definition = function(.Object) {
    file=config(.Object)$file
    return(pepr::Project(file))
  })


#' This method extracts the data from \code{\link{Project-class}} objects
#'
#' @param .Object An object of \code{\link{BiocProject-class}}
#'
#' @return a list with the data
#' 
#' @examples
#' ProjectConfig = system.file(
#' "extdata",
#' "example_peps-master",
#' "example_BiocProject",
#' "project_config.yaml",
#' package = "BiocProject"
#' )
#' 
#' bp = BiocProject(file=ProjectConfig)
#' getData(bp)
#' 
#' @export
setGeneric("getData", function(.Object, ...)
  standardGeneric("getData"))

#' @export
setMethod(
  f = "getData",
  signature = "BiocProject",
  definition = function(.Object) {
    return(.Object@.Data)
  })


