\name{R.to.Tcl}
\alias{R.to.Tcl}
\title{Convert a Character Vector to Tcl Format }
\description{
Converts a character vector into a brace-delimited Tcl list
}
\usage{
R.to.Tcl(character.vector)
}
\arguments{
  \item{character.vector}{A character vector}
}
\value{
A character vector of length 1
}
\author{David Firth, \email{david.firth@nuffield.ox.ac.uk}}
\seealso{\code{\link{Tcl.to.R}}}

\examples{
R.to.Tcl(c("apple","banana"))
}
\keyword{utilities}

