% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helper.R
\name{optimize_default}
\alias{optimize_default}
\title{Default optimization function}
\usage{
optimize_default(inst, self, private)
}
\arguments{
\item{inst}{\link{OptimInstance}}

\item{self}{\link{Optimizer}}

\item{private}{(\code{environment()})}
}
\value{
\link[data.table:data.table]{data.table::data.table}
}
\description{
Used internally in the \link{Optimizer}.
Brings together the private \code{.optimize()} method and the private \code{.assign_result()} method.
}
\keyword{internal}
