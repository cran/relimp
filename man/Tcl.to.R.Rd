\name{Tcl.to.R}
\alias{Tcl.to.R}
\title{Convert a Tcl List to R Character Vector}
\description{
Converts a brace-delimited list from Tcl into a character vector
}
\usage{
Tcl.to.R(tcl.list)
}
\arguments{
  \item{tcl.list}{a character string}
}
\value{
a character vector
}
\author{David Firth, \email{d.firth@warwick.ac.uk}}
\seealso{\code{\link{R.to.Tcl}}}

\examples{
Tcl.to.R("{apple} {banana} {pear}")
}
\keyword{utilities}

