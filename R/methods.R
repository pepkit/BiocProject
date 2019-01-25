
setGeneric("is.Project", function(.Object)
    standardGeneric("is.Project"))

#' @export
setMethod("is.Project","Annotated",function(.Object){
    mData = metadata(.Object)
    result = tryCatch(expr = {
        mData[[1]]
    }, error = function(e){
        FALSE
    })
    is(result,"Project")
})

setGeneric("getProject", function(.Object)
    standardGeneric("getProject"))

#' @export
setMethod("getProject","Annotated",function(.Object){
    if(is.Project(.Object)) {
        metadata(.Object)[[1]]
    } else {
        stop("This object does not have PEP in the metadata slot.")
    }
})

#' @export
setMethod(
    f = "samples",
    signature = "Annotated",
    definition = function(object) {
        samples(getProject(object))
    })


#' @export
setMethod(
    f = "config",
    signature = "Annotated",
    definition = function(object) {
        config(getProject(object))
    })

setGeneric("insertPEP",function(.Object, p) 
    standardGeneric("insertPEP"))

#' @export
insertPEP = function(object, p) {
    if(is(object, "Annotated")){
        metadata(object) = list(PEP=p)
        object
    }else{
        warning("The 'object' argument has to be of class 'Annotated', got '", class(object),"'.")
    }
}

#' @export
# setMethod(
#     f = "getSubsample",
#     signature = signature(
#         .Object = "Annotated",
#         sampleName = "character",
#         subsampleName = "character"
#     ),
#     definition = function(.Object, sampleName, subsampleName) {
#         if (!is.null(.Object@metadata$PEP) &
#             methods::is(object@metadata$PEP, "Project")) {
#             ret = pepr::getSubsample(
#                 .Object = .Object@metadata$PEP,
#                 sampleName = sampleName,
#                 subsampleName = subsampleName
#             )
#             return(ret)
#         } else{
#             return()
#         }
#     }
# )

BiocProject = function(config, autoLoad = T, func = NULL, funcArgs = NULL, ...) {
    p = pepr::Project(file = config, ...)
    # prevent PEP (Project object) input. This prevents BiocProject object
    # failing when the user provides the Project object
    if(is.null(funcArgs)){
      funcArgs = list()
    }else{
      pepArgs = as.logical(lapply(funcArgs, function(x) {
        is(x, "Project")
      }))
      if (any(pepArgs))
        funcArgs = funcArgs[-which(pepArgs)]
    }
    args = append(list(p), funcArgs)
    
    if (!is.null(func)) {
      # use the anonymous function if provided
      if (is.function(func)) {
        readData = .callBiocFun(func, list(p))
        return(insertPEP(readData, p))
      } else{
        stop("The anonymous function you provided is invalid.")
      }
    } else{
      # use config to find it
      if (autoLoad) {
        # check if the config consists of bioconductor section
        if(is.null(pepr::config(p)$bioconductor)){
          warning("The config YAML is missing the bioconductor section.")
            message("No data was read. Returning a Project object")
            return(p)
        }
        funcName = pepr::config(p)$bioconductor$read_fun_name
        # check if the function name was provided
        # and if it exists in the environment
        if (!is.null(funcName) && exists(funcName)) {
          # function from config.yaml in environment
          readData = .callBiocFun(funcName, args)
          message("Used function ", funcName, " from the environment")
          return(insertPEP(readData, p))
        } else{
          if (!is.null(funcName) && length(grep("(\\:){2,3}", funcName)) != 0) {
            # trying to access the function from the namespace that
            # was specified in the config.yaml read_fun_name
            splitted = strsplit(funcName, ":")[[1]]
            nonEmpty = splitted[which(splitted != "")]
            funcName = getFromNamespace(x=nonEmpty[2], ns=nonEmpty[1])
            readData = .callBiocFun(funcName, args)
            message("Used function ", funcName, " from the environment")
            return(insertPEP(readData, p))
          }
          # function from config.yaml in read_fun_name not in environment,
          # trying to source the file specified in
          # the config.yaml read_fun_path
          funcPath =
            pepr::.expandPath(pepr::config(p)$bioconductor$read_fun_path)
          if (!is.null(funcPath)){
            if (!file.exists(funcPath))
              funcPath = .makeAbsPath(funcPath,dirname(p@file))
              if(!file.exists(funcPath))
              stop(
                "The function does not exist in the environment and file ",
                funcPath,
                " does not exist"
              )
            readFun = source(funcPath)$value
            message("Function read from file: ", funcPath)
            readData = .callBiocFun(readFun, args)
            return(insertPEP(readData, p))
          }else{
            warning("Can't find function in the environment and the value for read_fun_path key was not provided in the config YAML.")
            message("No data was read. Returning a Project object")
            return(p)
          }
        }
      } else{
          message("No data was read. Returning a Project object")
          return(p)
      }
    }
}