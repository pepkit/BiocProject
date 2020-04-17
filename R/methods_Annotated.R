setGeneric(".is.project", function(.Object)
    standardGeneric(".is.project"))

setMethod(".is.project","Annotated",function(.Object){
    mData = S4Vectors::metadata(.Object)
    result = tryCatch(expr = {
        mData[[1]]
    }, error = function(e){
        FALSE
    })
    is(result,"Project")
})

#' Extract the object of \code{\link[pepr]{Project-class}} from 
#' the \code{\link[S4Vectors]{Annotated-class}} 
#'
#' This method can be used to extract the project metadata from objects of 
#' \code{\link[S4Vectors]{Annotated-class}} 
#'
#' @param .Object an object of \code{\link[S4Vectors]{Annotated-class}} 
#'
#' @return an object of \code{\link[pepr]{Project-class}}
#' 
#' @examples
#' projectConfig = system.file("extdata", "example_peps-cfg2",
#' "example_BiocProject", "project_config.yaml", package="BiocProject")
#' p=BiocProject(projectConfig)
#' getProject(p)
#'
#' @import S4Vectors
#' @exportMethod getProject
setGeneric("getProject", function(.Object)
    standardGeneric("getProject"))

#' @describeIn getProject extracts \code{\link[pepr]{Project-class}} from the \code{\link[S4Vectors]{Annotated-class}}
setMethod("getProject","Annotated",function(.Object){
    if(.is.project(.Object)) {
        S4Vectors::metadata(.Object)[[1]]
    } else {
        stop("This object does not have PEP in the metadata slot.")
    }
})

#' View samples in the objects of \code{\link[pepr]{Project-class}} 
#'
#' This method can be used to view the samples slot
#' of the \code{\link[pepr]{Project-class}} 
#' or \code{\link[S4Vectors]{Annotated-class}} 
#'
#' @param object an object of \code{\link[pepr]{Project-class}}
#'
#' @return a data.table with the with metadata about samples
#' @examples
#' projectConfig = system.file("extdata", "example_peps-cfg2",
#' "example_BiocProject", "project_config.yaml", package="BiocProject")
#' p=BiocProject(projectConfig)
#' sampleTable(p)
#' @import pepr
#' @export
setMethod(
    f = "sampleTable",
    signature = "Annotated",
    definition = function(object) {
        pepr::sampleTable(getProject(object))
    })


#' View PEP config of the object of \code{\link[pepr]{Project-class}}
#'
#' This method can be used to view the config slot of
#' the \code{\link[pepr]{Project-class}}
#'  or \code{\link[S4Vectors]{Annotated-class}} 
#'
#' @param object an object of \code{\link[pepr]{Project-class}}
#'
#' @return a list with the config file
#'
#' @examples
#' projectConfig = system.file("extdata", "example_peps-cfg2",
#' "example_BiocProject", "project_config.yaml", package="BiocProject")
#' p=BiocProject(projectConfig)
#' config(p)
#'
#' @import pepr
#' @export
setMethod(
    f = "config",
    signature = "Annotated",
    definition = function(object) {
        pepr::config(getProject(object))
    })

setGeneric("is", package = "methods")

#' Is an Object from a Class?
#' 
#' Functions to test inheritance relationships between an object and a class 
#' or between two classes. It uses the generic is function but overrides its 
#' behavior for obejcts of class \code{\link[S4Vectors]{Annotated-class}} when 
#' testing for inheritance from \code{\link[pepr]{Project-class}} class.
#' 
#' see the \code{\link[methods]{is}} for more details
#' 
#' @param object the object to be tested
#' @param class2 the class name to test the object against
#' 
#' @return a logical 
#' @examples
#' object = S4Vectors::List(test="test")
#' is(object,"Annotated")
#' 
#' @import methods
#' @export
setMethod("is", "Annotated", definition = function(object, class2){
    if(class2=="Project" & .is.project(object)){
        TRUE
    } else {
        methods::extends(class(object), class2)
    }
})

