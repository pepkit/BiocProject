# internal function used for wrapping the user-supplied function meessages 
# in a box
.wrapFunMessages = function(messages, type) {
          n = options("width")[[1]]
          header = ifelse(
                    length(messages) > 1,
                    paste0(" Your function ", type, "s (", length(messages), ") "),
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
.callBiocFun <- function(func, arguments) 
{ 
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
                    message("No data was read. Creating an empty BiocProject object...")
                    message("The error message was saved in the .Data slot.")
                    e$message
          } 
          res = withCallingHandlers(tryCatch(do.call(func, arguments), error = eHandler),warning = wHandler)
          if(length(.warnings) > 0){
                    warning("There were warnings associated with your function execution.")
                    .wrapFunMessages(.warnings,"warning")
          }
          return(res)
}

#' Create an absolute path from a primary target and a parent candidate.
#
#' @param perhapsRelative: Path to primary target directory.
#' @param  parent: Path to parent folder to use if target isn't absolute.
#
#' @return	Target itself if already absolute, else target nested within parent.
.makeAbsPath = function(perhapsRelative, parent) {
  if (!.isDefined(perhapsRelative)) { return(perhapsRelative)}
  perhapsRelative = pepr::.expandPath(perhapsRelative)
  if (.isAbsolute(perhapsRelative)) {
    abspath = perhapsRelative
  } else {
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


#' Determine whether a path is absolute.
#'
#' @param path The path to check for seeming absolute-ness.
#' @return Flag indicating whether the \code{path} appears to be absolute.
.isAbsolute = function(path) {
  if(!is.character(path)) stop("The path must be character.")
  firstChar = substr(path, 1, 1)
  return(identical("/", firstChar) | identical("~", firstChar))
}