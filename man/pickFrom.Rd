\name{pickFrom}
\alias{pickFrom}
\title{Pick Subsets from a Vector}
\description{
  Provides a Tk dialog for interactive selection of one or more subsets
  from a vector.  Requires the \code{tcltk} package.
}
\usage{
pickFrom(vec, nsets = 1, return.indices = FALSE, setlabels = NULL,
  title = "Subset picker",
  items.label = "Pick from:")
}
\arguments{
  \item{vec}{a vector}
  \item{nsets}{an integer, the number of subsets to be selected}
  \item{return.indices}{logical, whether indices (\code{TRUE}) or vector
    contents (\code{FALSE}) are to be returned}
  \item{setlabels}{a character vector of labels for the subsets}
  \item{title}{title of the Tk dialog window}
  \item{items.label}{a label for the set of items to be selected from}
}
\value{
  A list, with \code{nsets} components.  Each component is a
  selected sub-vector, or a numeric vector of indices for a
  selected sub-vector (if \code{return.indices} is \code{TRUE});
  the component names are as specified in \code{setlabels}, or interactively.
}
\author{David Firth, \email{d.firth@warwick.ac.uk}}

\examples{
## This cannot be run by example() but should be OK when pasted
## into an interactive R session
\dontrun{
pickFrom(c("apple", "banana", "plum", "grapefruit"), nsets = 2,
  setlabels = c("Fruits I like", "Fruits I tolerate"))
}}
\keyword{utilities}

