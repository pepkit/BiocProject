
#' A class representing a Portable Encapsulated Project and Summarized Experiment objects interface
#'
#' Provides a representation and functions to access project
#' configuration and sample annotation values for a PEP as well as functions concerning experimental results organization provided by the \linkS4class{RangedSummarizedExperiment} class.
#' Additionally, this class privides an interfece that connects them.
#'
#' This class inherits from classes \linkS4class{pepr::Project} and \linkS4class{SummarizedExperiment::RangedSummarizedExperiment}
#'
#' @slot ProjectSlots see \link[pepr]{Project} class for details
#' @slot SummarizedExperimentSlots see \link[SummarizedExperiment]{SummarizedExperiment} class docs for details
#'
#'
#' @exportClass BiocProject
setClass("BiocProject",
         # Inherits from Project and SummarizedExperiment objects
         contains = c("RangedSummarizedExperiment", "Project"))

#' A class representing a Portable Encapsulated Project and Summarized Experiment objects interface
#'
#' This is a helper that creates the `BiocProject` with empty \linkS4class{Project} and \linkS4class{SummarizedExperiment} objects included
#'
#' @inheritParams pepr::Project
#' @inheritParams SummarizedExperiment::SummarizedExperiment
#'
#' @export BiocProject
BiocProject <- function(file=character(), ...) {
  new("BiocProject", file=file, ...)
}
