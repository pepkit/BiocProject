

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
    do.call(selectMethod(f = "show", signature = "Project"), list(.Object))
  }
)

setGeneric("getColData", function(.Object, ...)
  standardGeneric("getColData"))

#' Get colData from the Project object (PEP)
#'
#' This method copies info about samples from \linkS4class{Project} object to \code{colData} slot on the \code{BiocProject} object.
#' 
#' There are 3 cases:
#' \itemize{
#'   \item The DataFrame in the samples slot is copied to the colData (when there is not colData and the number of columns in assays match the number of rows in samples)
#'   \item The DataFrame in the samples slot in merged with the colData DataFrame (when the colData slot is populate and their row numbers match)
#'   \item An error is thrown (when the number of rows in samples DataFrame does not match the number of columns in asssays or the number of rows in already populated colData)
#' }
#'
#' @param .Object An object of \linkS4class{BiocProject} class
#'
#' @return An object of \linkS4class{BiocProject} class. The colData slot is derived from samples attribute of \linkS4class{Project}
#' @examples 
#' projectConfig = system.file("extdata","example_peps-master","example_implied","project_config.yaml",package = "pepr")
#' nrows = 200
#' ncols = 6
#' counts = matrix(runif(nrows * ncols, 1, 1e4), nrows)
#' rowRanges = GRanges(rep(c("chr1", "chr2"), c(50, 150)),
#'                     IRanges(floor(runif(200, 1e5, 1e6)), width=100),
#'                     strand=sample(c("+", "-"), 200, TRUE),
#'                     feature_id=sprintf("ID%03d", 1:200))
#' colData = DataFrame(Treatment=rep(c("ChIP", "Input"), 3),
#'                     row.names=LETTERS[1:6])
#' bp = BiocProject(file=projectConfig, assays=list(counts=counts), rowRanges=rowRanges, colData=colData)
#' bpc = getColData(bp)
#' colData(bp)
#' colData(bpc)
#' @export getColData
setMethod(
  "getColData",
  signature = "BiocProject",
  definition = function(.Object) {
    if(NCOL(.Object@assays) != NROW(.Object@samples)) stop("The number of rows in Project samples (", NROW(.Object@samples),") and the number of samples in assays (", NCOL(.Object),") are not equal.")
    if (sum(dim(.Object@colData)) == 0){
      .Object@colData = DataFrame(.Object@samples)
    }else{
      if(NROW(.Object@colData) == NROW(DataFrame(.Object@samples))){
        .Object@colData = cbind(.Object@colData, DataFrame(.Object@samples))
      }else{
        stop("The number of rows in the current colData DataFrame (",NROW(.Object@colData),") is not equal the number of rows in the samples DataFrame (",NROW(DataFrame(.Object@samples)),"), cannot be concatenated")
      }
    }
    return(.Object)
  }
)

setGeneric("getMetadata", function(.Object, ...)
  standardGeneric("getMetadata"))

#' Get metadata from the Project object (PEP)
#'
#' This method copies the \href{https://pepkit.github.io/docs/project_config/}{PEP config file} data from \linkS4class{Project} object to a list in the metadata slot of the \code{BiocProject} object.
#'
#' @param .Object An object of \linkS4class{BiocProject} class
#'
#' @return .Object An object of \linkS4class{BiocProject} class. The metadata slot of \linkS4class{BiocProject} is enriched with data from \linkS4class{Project} object as an additional \code{PEP config file} field list in metadata slot.
#' @examples 
#' projectConfig = system.file("extdata","example_peps-master","example_implied","project_config.yaml",package = "pepr")
#' bp = BiocProject(file = projectConfig)
#' bpm = getMetadata(bp)
#' metadata(bp)
#' metadata(bpm)
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
