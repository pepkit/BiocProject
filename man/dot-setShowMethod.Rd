% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{.setShowMethod}
\alias{.setShowMethod}
\title{Redefine the show method of the object}
\usage{
.setShowMethod(returnedObject)
}
\arguments{
\item{returnedObject}{object of \code{\link[S4Vectors]{Annotated-class}}}
}
\value{
\code{FALSE} if the function was not set
}
\description{
Adds the Project objects display to the default show method 
of an \code{\link[S4Vectors]{Annotated-class}}
}
\details{
The method is defined in the environment in which the function was called, 
see: \code{\link[base]{sys.parent}}
}
\examples{
x = S4Vectors::List(c('so','cool'))
metadata(x) = list(PEP=pepr::Project())
.setShowMethod(x)
x
}
