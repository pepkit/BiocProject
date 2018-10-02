
setMethod(
  f = "initialize",
  signature = "BiocProject",
  definition = function(.Object, file, ...) {
    ellipsisArgs = list(...)
    .Object = callNextMethod(.Object, file)
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
#' @param .Object An object of BiocProject class
#'
#' @return .Object An object of \code{BiocProject} class. The colData slot is derived from samples attribute of \linkS4class{Project}
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
#' This method copies metadata from \linkS4class{Project} object to \linkS4class{MIAME} in the \code{BiocProject} object.
#'
#' @param .Object An object of BiocProject class
#'
#' @return .Object An object of BiocProject class. The metadata slot of \code{BiocProject} is enriched with data from \linkS4class{Project} object as an additional \code{PEP config file} field in the MIAME object.
#' @export getMetadata
setMethod(
  "getMetadata",
  signature = "BiocProject",
  definition = function(.Object) {
    # Check if MIAME object exists in the metadata section of SummarizedExperiment, create new if not
    if (is.list(metadata(.Object)) &
        length(metadata(.Object)) == 0) {
      metadata(.Object) = list(MIAME(other = list(`PEP config file` = .Object@config)))
      # Otherwise append existing one with the PEP file info
    } else{
      metadata(.Object) = lapply(metadata(.Object), function(x) {
        x = tryCatch({
          combine(x, MIAME(other = list(`PEP config file` = .Object@config)))
        }, error = function(e) {
          stop("The metadata section of the BiocProject object is not a MIAME object.")
        })
        return(x)
      })
    }
    return(.Object)
  }
)


setMethod("metadata",
          signature = "BiocProject",
          definition = function(x) {
            # gets the metadata into variable
            met=do.call(selectMethod(f = "metadata",signature = "SummarizedExperiment"),list(x))
            # if there is any metadata
            if(length(met)>0){
              # extract the PEP config data
              met=met[[1]]
              config=met@other$`PEP config file`
              met@other=list()
              # print the remaining part
              show(met)
              # print the config
              printNestedList(config)
            }else{
              # if no metadata, print empty list
              do.call(selectMethod(f = "metadata",signature = "SummarizedExperiment"),list(x))
            }
          })
