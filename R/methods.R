



#' @export
setMethod(
  f = "initialize",
  signature = "BiocProject",
  definition = function(.Object, autoLoad, func, funcArgs, ...) {
    .Object = do.call(selectMethod("initialize", signature="Project"),
                      list(.Object, ...))
    
    # internal function that wraps the external function execution
    # in tryCatch to indicate problems with the external function execution
    .callBiocFun = function(f, a) {
      readData = tryCatch({
        do.call(f, a)
      }, warning = function(w) {
        warning(
          "There are warnings associated with your function execution.
          \nCreating an empty BiocProject..."
        )
      }, error = function(e) {
        warning(
          "There are errors associated with your function execution.
          \nCreating an empty BiocProject..."
        )
      })
      return(readData)
      }
    
    if (!is.null(func)) {
      # use the lambda function if provided
      if (is.function(func)) {
        readData = .callBiocFun(func, list(.Object))
        .Object[[length(.Object)+1]] = readData
        return(.Object)
      } else{
        stop("The lambda function you provided is invalid.")
      }
    } else{
      # use config to find it
      if (autoLoad) {
        funcName = pepr::config(.Object)$bioconductor$read_fun_name
        if (exists(funcName)) {
          # function from config.yaml in environment
          readData = .callBiocFun(funcName, append(list(.Object), funcArgs))
          .Object[[length(.Object)+1]] = readData
          message("Used function ", funcName, " from the environment")
          return(.Object)
        } else{
          if (length(grep("(\\:){2,3}", funcName)) != 0) {
            # trying to access the function from the namespace that 
            # was specified in the config.yaml read_fun_name
            splitted = strsplit(funcName, ":")[[1]]
            nonEmpty = splitted[which(splitted != "")]
            funcName = getFromNamespace(nonEmpty[1], nonEmpty[2])
            readData = .callBiocFun(funcName, append(list(.Object), funcArgs))
            .Object[[length(.Object)+1]] = readData
            message("Used function ", funcName, " from the environment")
            return(.Object)
          }
          # function from config.yaml in read_fun_name not in environment,
          # trying to source the file specified in
          # the config.yaml read_fun_path
          funcPath =
            pepr::.expandPath(pepr::config(.Object)$bioconductor$read_fun_path)
          if (!file.exists(funcPath))
            stop(
              "The function does not exist in the environment and file ",
              funcPath ,
              " does not exist"
            )
          readFun = source(funcPath)$value
          readData = .callBiocFun(readFun, append(list(.Object), funcArgs))
          .Object[[length(.Object)+1]] = readData
          message("Function read from file: ", funcPath)
          return(.Object)
        }
      } else{
        message("No data was read. Creating an empty BiocProject object...")
        return(.Object)
      }
    }
  }
)

setMethod(
  f = "show",
  signature = "BiocProject",
  definition = function(object) {
    cat("data:\n")
    cat("BiocProject object. Class: ", class(object), fill=T)
    cat("  length: ", length(object), fill=T)
    cat("\nmetadata:\n")
    do.call(selectMethod("show", signature="Project"),
            list(object))
  }
)

#' Get \code{\link[pepr]{Project-class}} object
#'
#' This method coerces the \code{\link{BiocProject-class}}
#'  to \code{\link{Project-class}}
#'
#' @param .Object An object of \code{\link{BiocProject-class}}
#'
#' @return an object of \code{\link[pepr]{Project-class}} object
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
setGeneric("toProject", function(.Object)
  standardGeneric("toProject"))

#' @export
setMethod(
  f = "toProject",
  signature = "BiocProject",
  definition = function(.Object) {
    file = config(.Object)$file
    return(pepr::Project(file))
  }
)


#' Extract data from \code{\link[pepr]{Project-class}} objects
#'
#' This method extracts the data from \code{\link[pepr]{Project-class}} objects
#'
#' @param .Object An object of \code{\link{BiocProject-class}}
#'
#' @return a list with the data elements
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
setGeneric("getData", function(.Object)
  standardGeneric("getData"))

#' @export
setMethod(
  f = "getData",
  signature = "BiocProject",
  definition = function(.Object) {
    return(.Object@.Data)
  }
)
