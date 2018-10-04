

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
    # the sction below is a modified show method of SummarizedExperiment class. It displays the SummarizedExperiment part of the BiocProject object
    selectSome <- S4Vectors:::selectSome
    scat <- function(fmt,
                     vals = character(),
                     exdent = 2,
                     ...) {
      vals <- ifelse(nzchar(vals), vals, "''")
      lbls <- paste(S4Vectors:::selectSome(vals), collapse = " ")
      txt <- sprintf(fmt, length(vals), lbls)
      cat(strwrap(txt, exdent = exdent, ...), sep = "\n")
    }
    cat("class:", class(.Object), "\n")
    cat("dim:", dim(.Object), "\n")
    expt <-
      names(do.call(
        selectMethod(f = "metadata", signature = "RangedSummarizedExperiment"),
        list(.Object)
      ))
    if (is.null(expt))
      expt <-
      character(length(do.call(
        selectMethod(f = "metadata", signature = "RangedSummarizedExperiment"),
        list(.Object)
      )))
    scat("metadata(%d): %s\n", expt)
    nms <- assayNames(.Object)
    if (is.null(nms))
      nms <-
      character(length(assays(.Object, withDimnames = FALSE)))
    scat("assays(%d): %s\n", nms)
    dimnames <- dimnames(.Object)
    dlen <- sapply(dimnames, length)
    if (dlen[[1]])
      scat("rownames(%d): %s\n", dimnames[[1]])
    else
      scat("rownames: NULL\n")
    scat("rowData names(%d): %s\n", names(rowData(.Object)))
    if (dlen[[2]])
      scat("colnames(%d): %s\n", dimnames[[2]])
    else
      cat("colnames: NULL\n")
    scat("colData names(%d): %s\n", names(colData(.Object)))
    
    cat("\n")
    # calls the parent method of Project class. Displays the project part of the BiocProject object
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
    if (is.list(.Object@metadata) &
        length(.Object@metadata) == 0) {
      .Object@metadata = list(MIAME(other = list(`PEP config file` = .Object@config)))
      # Otherwise append existing one with the PEP file info
    } else{
      .Object@metadata = lapply(.Object@metadata, function(x) {
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

#' Display the metadata
#'
#' This method displays the metadata associated with hthe \code{BiocProject} object.
#'
#' @param x An object of \code{BiocProject} class
#'
#' @export metadata
setMethod(
  "metadata",
  signature = "BiocProject",
  definition = function(x) {
    # gets the metadata into variable
    metOri = do.call(selectMethod(f = "metadata", signature = "SummarizedExperiment"),
                     list(x))
    # if there is any metadata
    if (length(metOri) > 0) {
      # extract the PEP config data
      for (iMetadata in seq_along(metOri)) {
        met = metOri[[iMetadata]]
        if (!is.null(met@other$`PEP config file`))
          pepConfig = met@other$`PEP config file`
        met@other = list()
        # print the remaining part
        show(met)
      }
      # print the config
      cat("\nPEP config data\n")
      printNestedList(pepConfig)
    } else{
      # if no metadata, print empty list
      do.call(selectMethod(f = "metadata", signature = "SummarizedExperiment"),
              list(x))
      cat("No metadata to display, empty list returned.")
    }
    invisible(metOri)
  }
)
