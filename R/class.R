
#' Portable Encapsulated Project (PEP) class for biological applications
#'
#' This class provides a link between PEP and biological data structures
#'
#' @slot metadata see \link[S4Vectors]{Annotated} class for details. Meant to store an object of class \link[pepr]{Project}
#'
#' @importClassesFrom S4Vectors Annotated
#' 
#' @exportClass BiocProject

setClass("BiocProject",
         #Inherits from the Annotated class
         contains = "Annotated")

#' Portable Encapsulated Project (PEP) class for biological applications
#'
#' This is a helper that creates the \code{\link{BiocProject}} object
#' 
#' @param file a path to the config file
#' 
#' @return an object of \code{\link{BiocProject}} class
#' 
#' @seealso \url{https://pepkit.github.io/} 
#'
#' @export BiocProject
BiocProject <- function(file=character(), ...) {
  methods::new("BiocProject", file, ...)
}
