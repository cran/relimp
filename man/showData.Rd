\name{showData}
\alias{showData}
\title{Display a Data Frame in a Tk Text Widget}
\description{
  Prints the contents of a data frame to a Tk text window,
  for inspection.
}
\usage{
showData(dataframe, 
       colname.bgcolor = "royalblue", 
       rowname.bgcolor = "grey90", 
       body.bgcolor = "white", 
       colname.textcolor = "white", 
       rowname.textcolor = "darkred", 
       body.textcolor = "black", 
       font = "Courier 12", 
       maxheight = 30, 
       maxwidth = 80, 
       title = NULL,
       rowname.bar = "left",
       colname.bar = "top",
       rownumbers = FALSE, 
       placement = "-20-40")
}
\arguments{
  \item{dataframe}{A data frame}
  \item{colname.bgcolor}{A background colour for the 
               variable-names panel}
  \item{rowname.bgcolor}{A background colour for the row-names panel}
  \item{body.bgcolor}{A background colour for the data}
  \item{colname.textcolor}{A colour for the variable names}
  \item{rowname.textcolor}{A colour for the row names}
  \item{body.textcolor}{A colour for the data}
  \item{font}{The text font used -- should be a monospaced font}
  \item{maxheight}{The maximum number of rows to display}
  \item{maxwidth}{The maximum width of display, in characters}
  \item{title}{A title for the window.  Default is to use the name of
    the dataframe as given in the call to \code{showData()}}
  \item{rowname.bar}{position of sidebar for row names, \code{"left"}
    or \code{"right"}, or \code{c("left","right")}, or \code{NULL}}
  \item{colname.bar}{position of column names, \code{"top"} or
    \code{"bottom"}, or \code{c("top","bottom")}, or \code{NULL}}
  \item{rownumbers}{logical, whether row numbers should be displayed}
  \item{placement}{Position of the bottom right corner of the window}
}
\value{
  \code{invisible(NULL)}
}
\author{David Firth, \email{d.firth@warwick.ac.uk}}
\examples{
## This cannot be run by example() but should be OK when pasted
## into an interactive R session
\dontrun{
data(mtcars)
showData(mtcars)}
}
\keyword{utilities}
