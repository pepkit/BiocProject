


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

validBiocProject <- function(.Object) {
  ifelse(
    methods::is(.Object@metadata$PEP, "Project"),
    TRUE,
    "The first element of the list in the metadata slot is not of Project class."
  )
}

setValidity("BiocProject", validBiocProject)


#' Portable Encapsulated Project (PEP) class for biological applications
#'
#' This is a helper that creates the \code{\link{BiocProject}} object
#'
#' @param file a character vecotr with a path to the config file
#' @param subproject a character vector with a name of the subproject to be activated
#'
#' @return an object of \code{\link{BiocProject}} class
#'
#' @seealso \url{https://pepkit.github.io/}
#'
#' @export BiocProject
BiocProject <-
  function(file = character(),
           subproject = character()) {
    methods::new("BiocProject", file, subproject)
  }
