\name{vcov}
\alias{vcov.coxph}
\alias{vcov.survreg}
\alias{vcov.gls}
\alias{vcov.lme}

\title{Covariance Matrix of Model Estimates}
\description{
  \code{\link{vcov}} is a generic function from
  the \code{MASS} package, for 
  obtaining the estimated variance-covariance matrix of parameter
  estimates in a fitted model.  Methods are available at least for 
  models of class \code{lm}, \code{glm}, \code{nls}, \code{polr},
  \code{multinom}, \code{coxph}, \code{survreg}, \code{gls},
  \code{lme}.
}
\usage{
\method{vcov}{coxph}(object, ...)
}

\arguments{
  \item{object}{A fitted model object}
  \item{\dots}{Additional arguments for method functions (e.g., for
  models of class \code{glm}, the \code{dispersion} argument may sometimes 
  be used.}
}

\details{
  Extracts the estimated variance-covariance matrix
  of the estimator which produced
  the estimated coefficients in \code{coef(object)}.
}

\value{
  A matrix.
}

\author{David Firth \email{david.firth@nuffield.ox.ac.uk} }

\seealso{\code{\link{vcov}}}

\examples{
\dontrun{
library(MASS)
library(survival)
data(ovarian)
modl <- survreg(Surv(futime, fustat) ~ ecog.ps + rx,
                ovarian, dist='weibull',scale=1)
vcov(modl)}
}
\keyword{models}
\keyword{regression}
