\name{showData}
\alias{showData}
\title{Display a Data Frame in a Tk Text Widget}
\description{
  Displays the contents of a data frame in a modeless Tk text window,
  for inspection.  Objects not of class \code{data.frame},
  for example objects of class \code{table}, or \code{matrix},
  are coerced using \code{as.data.frame} prior to display.
}
\usage{
showData(dataframe, 
       colname.bgcolor = "grey50", 
       rowname.bgcolor = "grey50", 
       body.bgcolor = "white", 
       colname.textcolor = "white", 
       rowname.textcolor = "white", 
       body.textcolor = "black",
       font = "Courier 12", 
       maxheight = 30, 
       maxwidth = 80, 
       title = NULL,
       rowname.bar = "left",
       colname.bar = "top",
       rownumbers = FALSE, 
       placement = "-20-40",
       suppress.X11.warnings = TRUE)
}
\arguments{
  \item{dataframe}{A data frame, or an object to which
    \code{as.data.frame()} can be validly applied}
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
  \item{suppress.X11.warnings}{logical, if \code{TRUE} then any X11
    warnings are suppressed}
}
\value{
  Invisibly returns the Tk window containing the displayed data frame.
}
\note{An error results if the printed representation of \code{dataframe}
  exceeds the maximum allowed width of 10000 characters; see
  \code{\link{options}}.

  Text can be copied from the Tk window to the system clipboard, using
  <Control-C> or via a right-click pop-up menu.

  On some systems the window may take a few seconds to appear if the
  data frame is very large.
}
\author{David Firth, \email{d.firth@warwick.ac.uk}; with Rcmdr-specific features contributed by John Fox}
\examples{
## This cannot be run by example() but should be OK when pasted
## into an interactive R session
\dontrun{
data(mtcars)
showData(mtcars)}
}
\keyword{utilities}
