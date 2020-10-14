# internal function used for wrapping the user-supplied function meessages 
# in a box
.wrapFunMessages = function(messages, type) {
    n = options("width")[[1]]
    header = ifelse(
        length(messages) > 1,
        paste0(" Your function ", type,"s (", length(messages), ") "),
        paste0(" Your function ", type, " ")
    )
    nH = floor(nchar(header) / 2)
    nFill = floor(n / 2)
    message("\n",rep("-", nFill - nH), header, rep("-", nFill - nH))
    i = 1
    for (i in seq_along(messages)) {
        m = trimws(messages[i], which="both")
        message("\n", type, " ", i , ": ", m, "\n")
    }
    message(rep("-", n), "\n")
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
#' object = S4Vectors::List(result='test')
#' result = .insertPEP(object, pepr::Project())
#' metadata(result)
#' 
#' # If the object is not of class Annotated
#' object1 = 'test'
#' result1 = .insertPEP(object1, pepr::Project())
#' metadata(result1)
#' @import S4Vectors methods
#' @export
.insertPEP = function(object, pep) {
    if (!methods::is(pep, "Project")) 
        stop("the pep argument has to be of class 'Project', 
             got '", 
             class(pep), "'")
    # do we throw a warning/message
    # saying what happens in the
    # next line?
    if (methods::is(object, "list")) 
        object = S4Vectors::List(object)
    if (methods::is(object, "Annotated")) {
        S4Vectors::metadata(object) = 
            .unionList(S4Vectors::metadata(object), list(PEP = pep))
    } else {
        warning("BiocProject expects data loading functions to return an 
                'Annotated' object, but your function returned a '", 
                class(object), "' object. Therefore, this returned object has", 
                "been placed in the first slot of a S4Vectors::List")
        result = S4Vectors::List(result = object)
        S4Vectors::metadata(result) = list(PEP = pep)
        object = result
    }
    object
}

# Finds the pepr::Project
# object in a list and returns
# its index If it is not
# present, returns integer(0)
.findProjectInList = function(l) {
    which(as.logical(lapply(l, 
                            function(x) {
                                is(x, "Project")
                            })))
}

# internal function that wraps the external function execution
# in tryCatch to indicate problems with the external function execution
.callBiocFun <- function(func, arguments) { 
    if(!is(arguments, "list")) 
        stop("The 'arguments' argument has to be a list, got '",
            class(arguments),"'")
    .warnings = c()
    frameNumber <- sys.nframe()
    wHandler <- function(w){ 
        # warning handler 
        assign(".warnings", append(.warnings, w$message), 
        envir = sys.frame(frameNumber))
        invokeRestart("muffleWarning") 
    }
    eHandler <- function(e){
        # error handler 
        .wrapFunMessages(e$message,"error")
        stop("No data was read. The error message was returned instead: ", 
             e$message)
        S4Vectors::List(e$message)
    } 
    res = withCallingHandlers(
        tryCatch(do.call(func, arguments), error = eHandler),warning = wHandler)
    if(length(.warnings) > 0){
        warning("There were warnings associated with your function execution.")
        .wrapFunMessages(.warnings,"warning")
    }
    return(res)
}

# Create an absolute path from a primary target and a parent candidate.
#
# @param perhapsRelative: Path to primary target directory.
# @param parent a path to parent folder to use if target isn't absolute.
#
# @return Target itself if already absolute, else target nested within parent.
.makeAbsPath = function(perhapsRelative, parent) {
    if (!.isDefined(perhapsRelative)) return(perhapsRelative)
    perhapsRelative = pepr::.expandPath(perhapsRelative)
    if (.isAbsolute(perhapsRelative)) {
        abspath = perhapsRelative
    }else {
        abspath = file.path(normalizePath(parent), perhapsRelative)
    }
    if (!.isAbsolute(abspath)) {
        errmsg = sprintf("Relative path '%s' and parent '%s' failed to create
        absolute path: '%s'", perhapsRelative, parent, abspath)
        stop(errmsg)
    }
    return(abspath)
}

# Must test for is.null first, since is.na(NULL) returns a logical(0) which is
# not a boolean
.isDefined = function(var) { ! (is.null(var) || is.na(var)) }


# Determine whether a path is absolute.
#
# @param path The path to check for seeming absolute-ness.
# @return Flag indicating whether the \code{path} appears to be absolute.
.isAbsolute = function(path) {
    if(!is.character(path)) stop("The path must be character.")
    firstChar = substr(path, 1, 1)
    return(identical("/", firstChar) | identical("~", firstChar))
}

#' Update list with another list
#'
#' This function performs a union of two lists and updates the elements of the 
#' first one if are found in the other one. 
#' 
#' Both elements have to be lists. If some elements are not named, they are 
#' preserved but the order might be lost.
#' 
#' @param list1 a list to be updated
#' @param list2 a list to update with
#' @param combine a logical indicating whether the elements of the second list 
#' should replace (\code{FALSE}, default) or append to (\code{TRUE}) the 
#' first one.
#' 
#' @return an updated list
#' 
#' @examples 
#' list1=list(a=1,b=2)
#' list2=list(a=1,b=1,c=3)
#' .unionList(list1,list2)
#' 
#' @export
.unionList = function(list1, list2, 
                      combine = FALSE) {
    if ((!is.list(list1)) || (!is.list(list2))) 
        stop("One of the arguments is not a list")
    nms1 = names(list1)
    nms2 = names(list2)
    if (is.null(nms2)) 
        nms2 = ""
    counter = 1
    for (n in nms2) {
        idx = which(nms1 == n)
        if (length(idx) > 0) {
            if (combine) {
                list1[[idx]] = append(list1[[idx]], 
                                      list2[[n]])
            } else {
                list1[[idx]] = list2[[n]]
            }
        } else {
            add = list(list2[[counter]])
            names(add) = n
            list1 = append(list1, 
                           add)
        }
        counter = counter + 1
    }
    return(list1)
}

#' Redefine the show method of the object
#' 
#' Adds the Project objects display to the default show method 
#' of an \code{\link[S4Vectors]{Annotated-class}}
#' 
#' The method is defined in the environment in which the function was called, 
#' see: \code{\link[base]{sys.parent}}
#' 
#' @param returnedObject object of \code{\link[S4Vectors]{Annotated-class}}
#' 
#' @return \code{FALSE} if the function was not set
#'
#' @export
#'
#' @examples
#' x = S4Vectors::List(c('so','cool'))
#' metadata(x) = list(PEP=pepr::Project())
#' .setShowMethod(x)
#' x
.setShowMethod = function(returnedObject) {
    oriClass = class(returnedObject)
    if (!is(returnedObject, "Annotated")) {
        warning("The show method was not redefined for '", 
                oriClass, "'")
        return(FALSE)
    }
    oriShow = selectMethod("show", 
                           oriClass)
    # the new method is created
    # only if the environment of
    # the original one is locked.
    # this way the method will not
    # be redefined over and over
    # again when the BiocProject
    # functon is called.
    if (environmentIsLocked(environment(oriShow))) 
        setMethod("show", signature = oriClass, 
                  definition = function(object) {
                      do.call(oriShow, 
                              list(object))
                      pep = getProject(object)
                      cat("\nmetadata: ")
                      selectMethod("show", 
                                   "Project")(pep)
                  }, where = parent.frame())
}
