

setMethod(
  f = "initialize",
  signature = "BiocProject",
  definition = function(.Object, file, ...) {
    ellipsisArgs = list(...)
    .Object = callNextMethod(.Object, file = file)
    argsNames = names(ellipsisArgs)
    # Adds slots depending on the provided arguments in the constructor call
    if (any(argsNames == "assays"))
      .Object@assays = Assays(ellipsisArgs$assays)
    if (any(argsNames == "colData"))
      .Object@colData = ellipsisArgs$colData
    if (any(argsNames == "rowRanges")) {
      .Object@rowRanges = ellipsisArgs$rowRanges
      elementMetadata = S4Vectors:::make_zero_col_DataFrame(length(rowRanges))
      .Object@elementMetadata = elementMetadata
    }
    return(.Object)
  }
)

setMethod(
  f = "show",
  signature = "BiocProject",
  definition = function(.Object) {
    do.call(selectMethod(f = "show", signature = "RangedSummarizedExperiment"),
            list(.Object))
    cat("\n")
    #message
    do.call(selectMethod(f = "show", signature = "Project"), list(.Object))
  }
)

setGeneric("getColData", function(.Object, ...)
  standardGeneric("getColData"))

#' Get colData from the Project object (PEP)
#'
#' This method copies info about samples from \linkS4class{Project} object to \code{colData} slot on the \code{BiocProject} object.
#'
#' @param .Object An object of \linkS4class{BiocProject} class
#'
#' @return .Object An object of \linkS4class{BiocProject} class. The colData slot is derived from samples attribute of \linkS4class{Project}
#' @export getColData
setMethod(
  "getColData",
  signature = "BiocProject",
  definition = function(.Object) {
    # Check the compatibility - dimensions
    if (NCOL(.Object) != NROW(.Object@samples))
      stop(
        "The number of rows in Project samples (",
        NROW(.Object@samples),
        ") and SummarizedExperiment colData columns (",
        NCOL(.Object),
        ") are not equal."
      )
    colData(.Object) = DataFrame(.Object@samples)
    return(.Object)
  }
)

setGeneric("getMetadata", function(.Object, ...)
  standardGeneric("getMetadata"))

#' Get metadata from the Project object (PEP)
#'
#' This method copies the \href{https://pepkit.github.io/docs/project_config/}{TEST} data from \linkS4class{Project} object to a list in the metadata slot of the \code{BiocProject} object.
#'
#' @param .Object An object of \linkS4class{BiocProject} class
#'
#' @return .Object An object of \linkS4class{BiocProject} class. The metadata slot of \linkS4class{BiocProject} is enriched with data from \linkS4class{Project} object as an additional \code{PEP config file} field list in metadata slot.
#' @export getMetadata
setMethod(
  "getMetadata",
  signature = "BiocProject",
  definition = function(.Object) {
    met=do.call(selectMethod(f = "metadata", signature = "SummarizedExperiment"),list(.Object))
    metFinal=list('SummarizedExperiment metadata'=met,'PEP config file'=.Object@config)
    .Object@metadata=metFinal
    invisible(.Object)
    }
)
