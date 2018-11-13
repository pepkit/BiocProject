


#' Portable Encapsulated Project (PEP) class for biological applications
#'
#' This class provides a link between PEP and biological data structures.
#' 
#' Thi class can be created with the constructor: \code{\link{BiocProject}} 
#'
#' @slot .Data a list with the data. Can be extracted with \code{\link{getData}}
#' @slot file character vector path to config file on disk. 
#' @slot samples a data table object holding the sample metadata. Can be extracted with \code{\link{samples}}
#' @slot config a list object holding contents of the config file. Can be extracted with \code{\link{config}}
#'
#' @seealso \url{https://pepkit.github.io/}
#' 
#' @exportClass BiocProject
setClass("BiocProject",
         contains = c("list","Project"))

#' Portable Encapsulated Project (PEP) class for biological applications
#' 
#' This is a helper that creates the \code{\link{BiocProject-class}} object
#' 
#' If the \code{func} parameter is set to \code{TRUE} then the function name 
#' that will be used to read the data in is taken from the config slot 
#' in \code{\link[pepr]{Project}} 
#' (specifically: \code{config(project)$bioconductor$parse_code}).
#'  \cr If the \code{func} is set to string then the function of this name 
#'  will be used to read the data in.\cr If the \code{func} is set 
#'  to \code{FALSE} then an \code{\link{BiocProject}} object 
#'  with no data is created.
#' 
#' @param file a character vecotr with a path to the config file
#' @param subproject a character vector with a name of the subproject 
#' to be activated
#' @param func a boolean or a name of the function to use. 
#' See \code{Details} for more information
#' 
#' @return an object of \code{\link{BiocProject-class}}
#' 
#' @seealso \url{https://pepkit.github.io/}
#' 
#' @export BiocProject
BiocProject <- function(file = character(),subproject = character(), func=F) {
  methods::new("BiocProject", file=file, subproject=subproject, func=func)
}
