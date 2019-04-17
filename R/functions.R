
#' Portable Encapsulated Project (PEP) for biological applications
#'
#' This function creates a \code{\link[pepr]{Project-class}} object, 
#' and executes the user provided function with the created object 
#' as a first argument.
#' \cr\cr\emph{If the custom data processing function returns an object of 
#' class other than \code{\link[S4Vectors]{Annotated-class}}, the output 
#' will be packaged in a \code{\link[S4Vectors]{List-class}} with a metadata 
#' slot populated with the \code{\link[pepr]{Project-class}}.}
#'
#' This \code{\link{BiocProject}} function provides some degree 
#' of flexibility in your custom data processing function usage and 
#' implementation. Consider the possibilities listed below:
#' \itemize{
#'   \item use a function loaded into the \code{R} environment and specified in
#'   the config slot in \code{\link[pepr]{Project-class}}
#'   (specifically: \code{config(project)$bioconductor$readFunName}).
#'   \item use a function \emph{not} loaded into the \code{R} environment and 
#'   specified in the config slot in \code{\link[pepr]{Project}}
#'   (specifically: \code{config(project)$bioconductor$readFunPath}).
#'   \item use a function from other \code{R} package not loaded into 
#'   the \code{R} environment and specified in the config slot
#'   in \code{\link[pepr]{Project}} 
#'   (specifically: \code{config(project)$bioconductor$readFunName}), like:
#'   \code{pkgName::functionName}
#'   \item use a function implemented in the  \code{\link{BiocProject}}
#'   call (passed to the \code{func} argument - anonymous function). 
#'   This option is given the top priority and overrides other
#'    arguments if provided.
#' }
#' The custom data processing function must take 
#' the \code{\link[pepr]{Project-class}} as an argument since this object will 
#' be passed to the function by default. However, if the function requires
#' addtional arguments, ones can be provided with the \code{funcArgs} argument
#' in the \code{\link{BiocProject}} function call. 
#' Besides, the \code{func} argument with the anonymous 
#' function may serve similar possibility.
#' 
#' 
#' If the \code{autoLoad} is set to \code{FALSE} the data will not be loaded 
#' and empty \code{\link[pepr]{Project-class}} object will be returned.
#' 
#' 
#' @section Further reading: 
#' Browse the 
#' \href{http://code.databio.org/BiocProject/articles/index.html}{\code{BiocProject} package vignettes}
#' for more detailed explanation with examples.
#'
#'
#' @param file a character vector with a path to the PEP config file
#' @param subproject a character vector with a name of the subproject
#' to be activated
#' @param func a anonymous function that reads and/or processess the data, 
#' it must take 
#' the \code{\link[pepr]{Project-class}} as an argument.
#' See \code{Details} for more information
#' @param funcArgs a named list with arguments you want 
#' to pass to the \code{func}.
#'  The PEP will be passed automatically,
#'  but if provided regardless, the constructor will disregard it. 
#'  You can also pass the arguments in a \code{funcArgs} section within 
#'  the \code{bioconductor} section in the config file.
#' @param autoLoad a logical indicating whether the data should be loaded
#'  automatically. See \code{Details} for more information.
#'
#' @return an object of \code{\link[S4Vectors]{Annotated-class}} that is 
#' returned by the user provided function with 
#' the \code{\link[pepr]{Project-class}} object inserted into the first 
#' element of the list in its medatada slot 
#' 
#' @examples 
#' projectConfig = system.file("extdata", "example_peps-master",
#' "example_BiocProject", "project_config.yaml", package="BiocProject")
#' bp=BiocProject(projectConfig)
#' 
#' bp
#' 
#' metadata(bp)
#'
#' @seealso \url{https://pepkit.github.io/}
#' @import pepr
#' @export BiocProject
BiocProject = function(file, subproject = NULL, autoLoad = TRUE, func = NULL, 
                        funcArgs = NULL) {
    p = pepr::Project(file=file, subproject=subproject)
    # prevent PEP (Project object) input. This prevents BiocProject object
    # failing when the user provides the Project object
    if(is.null(funcArgs)){
        funcArgs = list()
    }else{
        if (length(.findProjectInList(funcArgs)) > 0) 
            funcArgs = funcArgs[-.findProjectInList(funcArgs)]
    }
    args = append(list(p), funcArgs)
    if(pepr::checkSection(pepr::config(p), c(MAIN_SECTION, FUNCTION_ARGS))){
        args = .updateList(config(p)[[MAIN_SECTION]][[FUNCTION_ARGS]],args)
        argsNames = names(args)
        project = args[[.findProjectInList(args)]]
        argsNames = append("",argsNames[-.findProjectInList(args)])
        args = append(list(p), args[[-.findProjectInList(args)]])
        names(args) = argsNames
    }
    
    if (!is.null(func)) {
        # use the anonymous function if provided
        if (is.function(func)) {
            readData = .callBiocFun(func, list(p))
            message("Used function from the 'func' argument")
            return(.insertPEP(readData, p))
        }else{
            stop("The anonymous function you provided is invalid.")
        }
    }else{
        # use config to find it
        if(!is.logical(autoLoad)) stop("'autoLoad' argument has to be a logical, 
                                    got '", class(autoLoad),"'")
        if (autoLoad) {
        # check if the config consists of MAIN_SECTION section
            if(!pepr::checkSection(pepr::config(p), MAIN_SECTION)){
                message("No data was read. Returning a Project object")
                warning("The config YAML is missing the '",
                        MAIN_SECTION,"' section.")
                return(p)
            }    
        funcName = pepr::config(p)[[MAIN_SECTION]][[FUNCTION_NAME]]
        # check if the function name was provided
        # and if it exists in the environment
            if (!is.null(funcName) && exists(funcName)) {
                # function from config.yaml in environment
                readData = .callBiocFun(funcName, args)
                message("Used function ", funcName, " from the environment")
                return(.insertPEP(readData, p))
            }else{
                if (!is.null(funcName) && length(grep("(\\:){2,3}", funcName)) != 0) {
                    # trying to access the function from the namespace that
                    # was specified in the config.yaml FUNCTION_NAME
                    splitted = strsplit(funcName, ":")[[1]]
                    nonEmpty = splitted[which(splitted != "")]
                    funcName = utils::getFromNamespace(nonEmpty[2], nonEmpty[1])
                    readData = .callBiocFun(funcName, args)
                    message("Used function ", funcName, " from the environment")
                    return(.insertPEP(readData, p))
                }
                # function from config.yaml in read_fun_name not in environment,
                # trying to source the file specified in
                # the config.yaml FUNCTION_PATH
                funcPath = pepr::.expandPath(
                    pepr::config(p)[[MAIN_SECTION]][[FUNCTION_PATH]])
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
                    return(.insertPEP(readData, p))
                }else{
                    warning("Can't find function in the environment and the value for '"
                            , FUNCTION_PATH,
                            "' key was not provided in the config YAML.")
                    message("No data was read. Returning a Project object")
                    return(p)
                }
            }
        }else{
            message("No data was read. Returning a Project object")
            return(p)
        }
    }
}

#' Insert a PEP metadata in a metadata slot of Annotated
#' 
#' This function inserts the PEP (\code{\link[pepr]{Project-class}}) 
#' into the metadata slot of objects that 
#' extend the \code{\link[S4Vectors]{Annotated-class}}
#' 
#' Additionally, if the object extends the 
#' \code{\link[S4Vectors]{Annotated-class}} (or is a list that will be
#' automatically converted to a \code{\link[S4Vectors]{List}}) the show method 
#' for its class is redefined to display the \code{\link[pepr]{Project-class}} 
#' as the metadata.
#' 
#' @param object an object of \code{\link[S4Vectors]{Annotated-class}}
#' @param pep an object of class \code{\link[pepr]{Project-class}}
#' 
#' @return an object of the same class as the object argument but enriched
#'  with the metadata from the pep argument
#' 
#' @examples 
#' # If the object is of class Annotated
#' object = S4Vectors::List(result="test")
#' result = .insertPEP(object, pepr::Project())
#' metadata(result)
#' 
#' # If the object is not of class Annotated
#' object1 = "test"
#' result1 = .insertPEP(object1, pepr::Project())
#' metadata(result1)
#' @import S4Vectors methods
#' @export
.insertPEP = function(object, pep) {
    if(!methods::is(pep, "Project")) 
        stop("the pep argument has to be of class 'Project', 
            got '", class(pep),"'")
    # do we throw a warning/message saying what happens in the next line?
    if(methods::is(object, "list"))
        object = S4Vectors::List(object)
    if(methods::is(object, "Annotated")){
        S4Vectors::metadata(object) = list(PEP=pep)
    } else{
        warning("To fully exploit features of BiocProject package, the object returned by the data processing function should be of class 'Annotated'. Got '",
                class(object),"' instead.")
        result = S4Vectors::List(result=object)
        S4Vectors::metadata(result) = list(PEP=pep)
        object = result
    }
    .setShowMethod(object)
    object
}
