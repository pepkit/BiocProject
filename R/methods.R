



#' @export
setMethod(
  f = "initialize",
  signature = "BiocProject",
  definition = function(.Object, autoLoad, func, funcArgs, ...) {
    .Object = do.call(selectMethod("initialize", signature="Project"),
                      list(.Object, ...))
    
    # prevent PEP (Project object) input. This prevents BiocProject object
    # failing when the user provides the Project object
    pepArgs = as.logical(lapply(funcArgs, function(x) {
      is(x, "Project")
    }))
    if (any(pepArgs))
      funcArgs = funcArgs[-which(pepArgs)]
    args = append(list(.Object), funcArgs)
    
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
        # check if the config consists of bioconductor section
        if(is.null(pepr::config(.Object)$bioconductor)){
          warning("The config YAML is missing the bioconductor section.")
          message("No data was read. Creating an empty BiocProject object...")
          return(.Object)
        }
        funcName = pepr::config(.Object)$bioconductor$read_fun_name
        # check if the function name was provided
        # and if it exists in the environment
        if (!is.null(funcName) && exists(funcName)) {
          # function from config.yaml in environment
          readData = .callBiocFun(funcName, args)
          .Object[[length(.Object)+1]] = readData
          message("Used function ", funcName, " from the environment")
          return(.Object)
        } else{
          if (!is.null(funcName) && length(grep("(\\:){2,3}", funcName)) != 0) {
            # trying to access the function from the namespace that 
            # was specified in the config.yaml read_fun_name
            splitted = strsplit(funcName, ":")[[1]]
            nonEmpty = splitted[which(splitted != "")]
            funcName = getFromNamespace(x=nonEmpty[2], ns=nonEmpty[1])
            readData = .callBiocFun(funcName, args)
            .Object[[length(.Object)+1]] = readData
            message("Used function ", funcName, " from the environment")
            return(.Object)
          }
          # function from config.yaml in read_fun_name not in environment,
          # trying to source the file specified in
          # the config.yaml read_fun_path
          funcPath =
            pepr::.expandPath(pepr::config(.Object)$bioconductor$read_fun_path)
          if (!is.null(funcPath)){
            if (!file.exists(funcPath))
              funcPath = .makeAbsPath(funcPath,dirname(.Object@file))
              if(!file.exists(funcPath))
              stop(
                "The function does not exist in the environment and file ",
                funcPath,
                " does not exist"
              )
            readFun = source(funcPath)$value
            readData = .callBiocFun(readFun, args)
            .Object[[length(.Object)+1]] = readData
            message("Function read from file: ", funcPath)
            return(.Object)
          }else{
            warning("Can't find function in the environment and the value for read_fun_path key was not provided in the config YAML.")
            message("No data was read. Creating an empty BiocProject object...")
            return(.Object)
          }
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
#' @param .Object an object of \code{\link{BiocProject-class}}
#' @param subproject a character with the name of the subproject
#' that should be activated during the converstion 
#' to the \code{\link[pepr]{Project-class}} object
#'
#' @return an object of \code{\link[pepr]{Project-class}} object
#' 
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
setGeneric("toProject", function(.Object,subproject=character())
  standardGeneric("toProject"))

#' @export
setMethod(
  f = "toProject",
  signature = "BiocProject",
  definition = function(.Object,subproject) {
    pepr::Project(file=.Object@file,subproject=subproject)
  }
)


#' Extract data from \code{\link[pepr]{Project-class}} objects
#'
#' This method extracts the data from \code{\link[pepr]{Project-class}} objects
#'
#' @param .Object an object of \code{\link{BiocProject-class}}
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