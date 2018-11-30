.wrapFunMessages = function(str, type) {
  str = trimws(str, which = "both")
  n = options("width")[[1]]
  header = paste0(" Your function ", type, " ")
  nH = floor(nchar(header) / 2)
  nFill = floor(n / 2)
  message(rep("-", nFill - nH), header, rep("-", nFill - nH))
  message("\n", str, "\n")
  message("\n", rep("-", n), "\n")
}
# internal function that wraps the external function execution
# in tryCatch to indicate problems with the external function execution
.callBiocFun = function(f, a) {
  readData = tryCatch({
    do.call(f, a)
  }, warning = function(w) {
    warning(
      "There are warnings associated with your function execution."
    )
    .wrapFunMessages(w$message,"warning")
    message("No data was read. Creating an empty BiocProject object...")
    return(w$message)
  }, error = function(e) {
    warning(
      "There are errors associated with your function execution."
    )
    .wrapFunMessages(e$message,"error")
    message("No data was read. Creating an empty BiocProject object...")
    return(e$message)
  })
  return(readData)
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