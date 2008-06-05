\name{pickFrom}
\alias{pickFrom}
\title{Pick Subsets from a Vector}
\description{
  Provides a Tk dialog or a text-based menu for interactive selection of
  one or more subsets from a vector.
}
\usage{
pickFrom(vec, nsets = 1, return.indices = FALSE,
                 setlabels = NULL, edit.setlabels = TRUE,
                 subset = TRUE,
                 warningText = "one or more selections empty",
                 title = "Subset picker",
                 items.label = "Pick from",
                 labels.prompt = "Your label for this set",
                 list.height = 20,
                 items.scrollbar = TRUE,
                 preserve.order = TRUE,
                 graphics = TRUE,
                 listFont = "Courier 12",
                 labelFont = "Helvetica 11",
                 windowPos = "+150+30")
}
\arguments{
  \item{vec}{a vector}
  \item{nsets}{a positive integer, the number of subsets to be selected}
  \item{return.indices}{logical, whether indices (\code{TRUE}) or vector
    contents (\code{FALSE}) are to be returned}
  \item{setlabels}{a character vector of labels for the subsets}
  \item{edit.setlabels}{logical, determines whether a textbox is
    provided for editing the label of each subset}
  \item{subset}{logical, character  or numeric vector indicating which
    elements of \code{vec} should be made available for selection.  Default is
    to make all elements available.}
  \item{warningText}{character, text to use as a warning in situations
    where no selection is made into one or more of the specified sets}
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
  \item{graphics}{logical: should a dialog be used, if possible?}
  \item{listFont}{a Tk font specification for the items list and subsets}
  \item{labelFont}{a Tk font specification for the labels entrybox}
  \item{windowPos}{position of the Tk dialog, in pixels from top left of
    display}
}
\details{
  If \code{graphics = TRUE} and the \code{tcltk} package is operational,
  a Tk dialog is used, otherwise a text menu. 
  
  If \code{return.indices} is used together with \code{subset}, the
  indices returned relate to \code{vec}, not to \code{vec[subset]}.
}
\value{
  EITHER (in the case of a text menu or if the dialog is ended with "OK")
  a list, with \code{nsets} components.  Each component is a
  selected sub-vector, or a numeric vector of indices for a
  selected sub-vector (if \code{return.indices} is \code{TRUE}).
  The component names are as specified in \code{setlabels}, or
  as specified interactively.

  OR (if the dialog is ended either "Cancel" or the close-window control
  button is used) 
  \code{NULL}.
}
\author{David Firth, with contributions from Heather Turner}

\examples{
## These examples cannot be run by example() but should be OK when pasted
## into an interactive R session
\dontrun{
pickFrom(c("apple", "banana", "plum", "grapefruit"), nsets = 2,
  preserve.order = FALSE,
  setlabels = c("Fruits I like", "Fruits I tolerate"))
}
\dontrun{
## Type selections as e.g. 1:2, 4
pickFrom(c("apple", "banana", "plum", "grapefruit"), nsets = 2,
  preserve.order = FALSE,
  setlabels = c("Fruits I like", "Fruits I tolerate"),
  graphics = FALSE)
}}
\keyword{utilities}

