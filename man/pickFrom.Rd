\name{pickFrom}
\alias{pickFrom}
\title{Pick Subsets from a Vector}
\description{
  Provides a Tk dialog for interactive selection of one or more subsets
  from a vector.  Requires the \code{tcltk} package.
}
\usage{
pickFrom(vec, nsets = 1, return.indices = FALSE,
                 setlabels = NULL, edit.setlabels = TRUE,
                 title = "Subset picker",
                 items.label = "Pick from:",
                 labels.prompt = "Your label for this set:",
                 list.height = 20,
                 items.scrollbar = TRUE,
                 preserve.order = TRUE,
                 listFont = "Courier 12",
                 labelFont = "Helvetica 11",
                 windowPos = "+150+30")
}
\arguments{
  \item{vec}{a vector}
  \item{nsets}{an integer, the number of subsets to be selected}
  \item{return.indices}{logical, whether indices (\code{TRUE}) or vector
    contents (\code{FALSE}) are to be returned}
  \item{setlabels}{a character vector of labels for the subsets}
  \item{edit.setlabels}{logical, determines whether a textbox is
    provided for editing the label of each subset}
  \item{title}{character, title of the Tk dialog window}
  \item{items.label}{character,
    a label for the set of items to be selected from}
  \item{labels.prompt}{character, a prompt for textual set label(s)}
  \item{list.height}{maximum number of elements of \code{vec} to display
    at once}
  \item{items.scrollbar}{logical, whether a scrollbar is to be provided when
    \code{vec} is longer than \code{list.height}}
  \item{preserve.order}{logical: should the order of items in \code{vec}
    be maintained in all of the returned subsets?}
  \item{listFont}{a Tk font specification for the items list and subsets}
  \item{labelFont}{a Tk font specification for the labels entrybox}
  \item{windowPos}{position of the Tk dialog, in pixels from top left of
    display}
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
  preserve.order = FALSE,
  setlabels = c("Fruits I like", "Fruits I tolerate"))
}}
\keyword{utilities}

