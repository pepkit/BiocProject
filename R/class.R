
#' A class representing a Portable Encapsulated Project and Summarized Experiment objects interface
#'
#' Provides a representation and functions to access project
#' configuration and sample annotation values for a PEP as well as functions concerning experimental results organization provided by the \linkS4class{RangedSummarizedExperiment} class.
#' Additionally, this class privides an interfece that connects them.
#'
#' This class inherits frusom classes \linkS4class{Project} and \linkS4class{RangedSummarizedExperiment}
#'
#' @inheritParams SummarizedExperiment
#'
#' @exportClass BiocProject
setClass("BiocProject",
         # Inherits from Project and SummarizedExperiment objects
         contains = c("RangedSummarizedExperiment", "Project"))

#' A class representing a Portable Encapsulated Project and Summarized Experiment objects interface
#' This is a helper that creates the BiocProject with empty Project and SummarizedExperiment objects included
#'
#' @param file a string with a path to the config file as in \linkS4class{Project}
#'
#' @export BiocProject
BiocProject <- function(file=character(), ...) {
  new("BiocProject", file=file, ...)
}
