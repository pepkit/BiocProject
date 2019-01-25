setGeneric("is.Project", function(.Object)
    standardGeneric("is.Project"))

#' @export
setMethod("is.Project","Annotated",function(.Object){
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
