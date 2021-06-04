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
#' additional arguments, ones can be provided with the \code{funcArgs} argument
#' in the \code{\link{BiocProject}} function call. 
#' Besides, the \code{func} argument with the anonymous 
#' function may serve similar possibility.
#' 
#' 
#' If the \code{autoLoad} is set to \code{FALSE} the data will not be loaded 
#' and empty \code{\link[pepr]{Project-class}} object will be returned.
#' 
#' @note The \code{bioconductor} section can be read from the project config 
#' file or pipeline interface. The former is given the priority
#' 
#' @section Further reading: 
#' Browse the 
#' \href{http://code.databio.org/BiocProject/articles/index.html}{\code{BiocProject} package vignettes}
#' for more detailed explanation with examples.
#'
#' @param file a character vector with a path to the PEP config file
#' @param amendments a character vector with a name of the amendments
#' to be activated
#' @param func a anonymous function that reads and/or processes the data, 
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
#' @param projectLevel logical indicating whether a only project-level pifaces 
#'  should be considered. Otherwise, only sample-level ones are. 
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
BiocProject = function(file, amendments = NULL, autoLoad = TRUE, func = NULL,
                       projectLevel = FALSE, funcArgs = NULL) {
    p = pepr::Project(file=file, amendments = amendments)
    # prevent PEP (Project object) input. This prevents BiocProject object
    # failing when the user provides the Project object
    if(is.null(funcArgs)){
        funcArgs = list()
    }else{
        if (length(.findProjectInList(funcArgs)) > 0) {
            warning("Project object was found in the arguments list. 
                    It will be removed.")
            funcArgs = funcArgs[-.findProjectInList(funcArgs)]
        }
    }
    args = append(list(p), funcArgs)
    cfg = .getBiocConfig(p, projectLevel)
    if(is.null(cfg))
        cfg = pepr::config(p)
    if(pepr::.checkSection(cfg, c(BIOC_SECTION, FUNCTION_ARGS))){
        args = .unionList(config(p)[[BIOC_SECTION]][[FUNCTION_ARGS]], args)
        argsNames = names(args)
        project = args[[.findProjectInList(args)]]
        argsNames = append("",argsNames[-.findProjectInList(args)])
        args = append(list(p), args[-.findProjectInList(args)])
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
        if(!is.logical(autoLoad)) stop("'autoLoad' argument has to be a",
                                       " logical, got '", class(autoLoad),"'")
        if (autoLoad) {
            # check if the config consists of BIOC_SECTION section
            if(!pepr::.checkSection(cfg, BIOC_SECTION)){
                message("No data was read. Returning a Project object")
                warning("The config YAML is missing the '",
                        BIOC_SECTION,"' section.")
                return(p)
            }    
            funcName = cfg[[BIOC_SECTION]][[FUNCTION_NAME]]
            # check if the function name was provided
            # and if it exists in the environment
            if (!is.null(funcName) && exists(funcName)) {
                # function from config.yaml in environment
                readData = .callBiocFun(funcName, args)
                message("Used function '", funcName, "' from the environment")
                return(.insertPEP(readData, p))
            }else{
                if (!is.null(funcName) && 
                    length(grep("(\\:){2,3}", funcName)) != 0) {
                    # trying to access the function from the namespace that
                    # was specified in the config.yaml FUNCTION_NAME
                    splitted = strsplit(funcName, ":")[[1]]
                    nonEmpty = splitted[which(splitted != "")]
                    funcCode = utils::getFromNamespace(nonEmpty[2], nonEmpty[1])
                    readData = .callBiocFun(funcCode, args)
                    message("Used function '", nonEmpty[2],
                            "' from the package: ", nonEmpty[1])
                    return(.insertPEP(readData, p))
                }
                # function from config.yaml in FUNCTION_NAME not in environment,
                # trying to source the file specified 
                # in the config.yaml FUNCTION_PATH
                funcPath = pepr::.expandPath(
                    cfg[[BIOC_SECTION]][[FUNCTION_PATH]])
                if (!is.null(funcPath)){
                    if (!file.exists(funcPath))
                        funcPath = .makeAbsPath(funcPath, dirname(p@file))
                    if (!file.exists(funcPath))
                        stop("The function does not exist in the environment", 
                             " and file '", funcPath, "' does not exist")
                    # Load the sourced objects into a new environment, 
                    # so they are not in the .GlobalEnv after the BiocProject 
                    # function execution
                    e = new.env()
                    # only the last defined function is saved into the variable 
                    # below so first we need to check whether the FUNCTION_NAME 
                    # defines a preferred function. If it does not, then use the
                    # lastFun. This is relevant in case multiple functions are
                    # defined in the file specified in FUNCTION_PATH.
                    lastFun = source(funcPath, local=e)$value
                    # check again for the specified funcion name, maybe it is 
                    # defined in the file which was just sourced
                    if (!is.null(funcName) && exists(funcName, where=e)) {
                        message("Function '", funcName,"' read from file '", 
                                funcPath, "'")
                        readData = .callBiocFun(
                            getFunction(funcName, where=e,mustFind=TRUE), args)
                        return(.insertPEP(readData, p))
                    }
                    # the function indicated in FUNCTION_NAME was not found, 
                    # use the last one in FUNCTION_PATH
                    message("Multiple functions found in '", funcPath, 
                            "'. Using the last one.")
                    readData = .callBiocFun(lastFun, args)
                    return(.insertPEP(readData, p))
                }else{
                    warning("Can't find function in the environment and the", 
                            " value for '" , FUNCTION_PATH, 
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