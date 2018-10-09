
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
#' This is a helper that creates the `BiocProject`
#'
#' @export BiocProject
BiocProject <- function(...) {
  new("BiocProject", ...)
}
