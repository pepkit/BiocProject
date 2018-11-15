


#' @export
setMethod(
  f = "initialize",
  signature = "BiocProject",
  definition = function(.Object, autoLoad, func, funcArgs, ...) {
    .Object = do.call(selectMethod("initialize", signature = "Project"),
                      list(.Object, ...))
    if(!is.null(func)){
      # use the lambda function if provided
      if(is.function(eval(substitute(func)))){
        sFunc=substitute(func)
        readFun = eval(sFunc)
        data=do.call(readFun,list(.Object))
        .Object[[length(.Object) + 1]] = data
        return(.Object)
      }else{
        stop("The lambda function you provided is invalid.")
      }
    }else{
      # use config to find it
      if(autoLoad){
        funcName = pepr::config(.Object)$bioconductor$read_fun_name
        if(exists(funcName)){
          # function from config.yaml in environment
          readFun=funcName
          data=do.call(readFun,append(list(.Object),funcArgs))
          .Object[[length(.Object) + 1]] = data
          message("Used function ",funcName," from environment")
          return(.Object)
        }else{
          # function from config.yaml in read_fun_name not in environment,
          # tring to source the file specified in 
          # the config.yaml read_fun_path
          funcPath = 
            pepr::.expandPath(pepr::config(.Object)$bioconductor$read_fun_path)
          message("Function read from file: ",funcPath)
          if(!file.exists(paste0(funcPath))) stop(
            "The function ",
            funcName,
            " does not exist in the environment and file ",funcPath ," does not exist",
            getwd()
          )
          readFun=source(funcPath)$value
          data=do.call(readFun,append(list(.Object),funcArgs))
          .Object[[length(.Object) + 1]] = data
          return(.Object)
        }
      } else{
        message("No data was read. Creating an empty BiocProject object...")
        return(.Object)
      }
    }})

setMethod(
  f = "show",
  signature = "BiocProject",
  definition = function(object) {
    cat("BiocProject object. Class: ", class(object), fill = T)
    cat("  length: ", length(object), fill = T)
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
