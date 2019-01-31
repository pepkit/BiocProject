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
        assign(".warnings", append(.warnings,w$message), 
        envir = sys.frame(frameNumber))
        invokeRestart("muffleWarning") 
    }
    eHandler <- function(e){
        # error handler 
        .wrapFunMessages(e$message,"error")
        message("No data was read. The error message was returned instead.")
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
#' 
#' @return an updated list
#' 
#' @examples 
#' list1=list(a=1,b=2)
#' list2=list(a=1,b=1,c=3)
#' .updateList(list1,list2)
#' 
#' @export
.updateList = function(list1,list2) {
    if((!is.list(list1)) || (!is.list(list2)))
        stop("One of the arguments was not a list")
    nms1 = names(list1)
    nms2 = names(list2)
    for(n in nms2){
        idx = which(nms1 == n)
        if(length(idx) > 0){
            list1[[idx]] = list2[[n]]
        }else{
            add = list2[[n]]
            names(add) = n
            list1 = c(list1,add)
        }
    }
    return(list1)
}
