



#' Portable Encapsulated Project (PEP) class for biological applications
#'
#' This class provides a link between PEP and biological data structures.
#'
#' Thi class can be created with the constructor: \code{\link{BiocProject}}
#'
#' @slot .Data a list with the data. Can be extracted with \code{\link{getData}}
#' @slot file character vector path to config file on disk.
#' @slot samples a data table object holding the sample metadata.
#'  Can be extracted with \code{\link[pepr]{samples}}
#' @slot config a list object holding contents of the config file.
#'  Can be extracted with \code{\link[pepr]{config}}
#'
#' @seealso \url{https://pepkit.github.io/}
#'
#' @exportClass BiocProject
setClass("BiocProject",
         contains = c("list", "Project"))

#' Portable Encapsulated Project (PEP) class for biological applications
#'
#' This is a helper that creates the \code{\link{BiocProject-class}} object
#'
#' This \code{\link{BiocProject-class}} object constructor provides some level 
#' of flexibility in your custom data processing function usage and 
#' implementation. Consider the possibilities listed below:
#' \itemize{
#'   \item use a function loaded into the \code{R} environment and specified in
#'   the config slot in \code{\link[pepr]{Project}}
#'   (specifically: \code{config(project)$bioconductor$read_fun_name}).
#'   \item use a function \emph{not} loaded into the \code{R} environment and specified in
#'   the config slot in \code{\link[pepr]{Project}}
#'   (specifically: \code{config(project)$bioconductor$read_fun_path}).
#'   \item use a function from other \code{R} package not loaded into 
#'   the \code{R} environment and specified in the config slot
#'   in \code{\link[pepr]{Project}} 
#'   (specifically: \code{config(project)$bioconductor$read_fun_name}), like:
#'   \code{pkgName::functionName}
#'   \item use a function implemented in the  \code{\link{BiocProject}}
#'   call (passed to the \code{func} argument - anonymous function). This option is given the top priority and overrides 
#'   other arguments if provided.
#' }
#' The custom data processing function must take 
#' the \code{\link[pepr]{Project-class}} as an argument since this object will 
#' be passed to the function by default. However, if the function requires
#' addtional arguments, ones can be provided with the \code{funcArgs} argument
#' in the \code{\link{BiocProject}} function call. 
#' Besides, the \code{func} argument with the anonymous function may serve similar
#' possibility.
#' 
#' If the \code{autoLoad} is set to \code{FALSE} the data will not be loaded 
#' and empty \code{\link{BiocProject-class}} object will be returned.
#' 
#' 
#' @section Further reading: 
#' Browse the 
#' \href{http://code.databio.org/BiocProject/articles/index.html}{\code{BiocProject} package vignettes}
#' for more detailed explanation with examples.
#'
#'
#' @param file a character vector with a path to the config file
#' @param subproject a character vector with a name of the subproject
#' to be activated
#' @param func a anonymous function that reads and/or processess the data, it must take 
#' the \code{\link[pepr]{Project-class}} as an argument.
#' See \code{Details} for more information
#' @param funcArgs a named list with arguments you want to pass to the \code{func}.
#'  The PEP will be passed automatically,
#'  but if provided regardless, the constructor will disregard it
#' @param autoLoad a logical indicating whether the data should be loaded
#'  automatically. See \code{Details} for more information.
#'
#' @return an object of \code{\link{BiocProject-class}}
#'
#' @seealso \url{https://pepkit.github.io/}
#'
#' @export BiocProject
BiocProject <-
  function(file = character(),
           subproject = character(),
           autoLoad = TRUE,
           func = NULL,
           funcArgs = list()) {
    if (!is.logical(autoLoad))
      stop("The autoLoad argument is not logical.")
    if (!is.list(funcArgs))
      stop("The funcArgs has to be a named list.")
    if (length(funcArgs) > 0 && is.null(names(funcArgs)))
      stop("The funcArgs has to be a named list")
    
    methods::new(
      "BiocProject",
      file = file,
      subproject = subproject,
      func = func,
      funcArgs = funcArgs,
      autoLoad = autoLoad
    )
  }
