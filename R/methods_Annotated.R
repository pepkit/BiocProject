setGeneric(".is.project", function(.Object)
    standardGeneric(".is.project"))

setMethod(".is.project","Annotated",function(.Object){
    mData = metadata(.Object)
    result = tryCatch(expr = {
        mData[[1]]
    }, error = function(e){
        FALSE
    })
    is(result,"Project")
})

setGeneric("getProject", function(.Object)
    standardGeneric("getProject"))

#' @export
setMethod("getProject","Annotated",function(.Object){
    if(is.Project(.Object)) {
        metadata(.Object)[[1]]
    } else {
        stop("This object does not have PEP in the metadata slot.")
    }
})

#' @export
setMethod(
    f = "samples",
    signature = "Annotated",
    definition = function(object) {
        samples(getProject(object))
    })

#' @export
setMethod(
    f = "config",
    signature = "Annotated",
    definition = function(object) {
        config(getProject(object))
    })

setGeneric("is", package = "methods")

#' @export
setMethod("is", "Annotated", definition = function(object, class2){
    if(class2=="Project" & .is.project(object)){
        TRUE
    } else {
        extends(class(object), class2)
    }
})

