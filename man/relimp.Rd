\name{relimp}
\alias{relimp}
\alias{print.relimp}

\title{Relative Importance of Predictors in a Regression Model}
\description{
     Produces a summary
     of the relative importance of two predictors or two sets of predictors
     in a fitted model object.
}
\usage{
relimp(object, set1=NULL,  set2=NULL, label1="set1", label2="set2", 
          subset=TRUE, 
          response.cat=NULL, ...)
print.relimp(x, digits=3, ...)
}

\arguments{
  \item{object}{A model object of class
    \code{\link{lm}}, \code{\link{glm}}, \code{\link[survival]{coxph}},
    \code{\link[survival]{survreg}}, 
    \code{\link[nnet]{multinom}}, 
    \code{\link[MASS]{polr}}, \code{\link[nlme]{gls}} or \code{\link[nlme]{lme}}}
  \item{set1}{An index or vector of indices for the effects to be
    included in the numerator of the comparison}
  \item{set2}{An index or vector of indices for the effects to be
    included in the denominator of the comparison}
  \item{label1}{A character string; mnemonic name for the 
    variables in \code{set1}}
  \item{label2}{A character string; mnemonic name for the
    variables in \code{set2}}
  \item{subset}{Either a vector of numeric indices for the cases to be included
    in the standardization of effects, or a vector of logicals
    (\code{TRUE} for inclusion)
    whose length is the same as the number of rows in the model frame,
    \code{object$model}.
    The default choice is to include all cases in the model frame.}
  \item{response.cat}{If \code{object} is of class \code{multinom}, 
    this is a character
    string used to specify which regression is of interest (i.e., the
    regression
    which predicts the log odds on \code{response cat} versus the model's 
    reference category).  The \code{response.cat} argument should be an
    element of
    \code{object$lab}; or \code{NULL} if \code{object} is not of class
    \code{multinom}.}
  \item{\dots}{For models of class \code{glm}, one may additionally set
    the dispersion parameter for the family (for example,
    \code{dispersion=1.69}). By default
    it is obtained from \code{object}.  Supplying it here
    permits explicit allowance for over-dispersion, for example.}
  \item{x}{an object of class \code{relimp}}
  \item{digits}{The number of decimal places to be used in the printed 
    summary.  Default is 3.}
}

\details{If \code{set1} and \code{set2} both have length 1, relative importance is
  measured by the ratio of the two standardized coefficients.
  Equivalently this is the ratio of the standard deviations of the two
  contributions to the linear predictor, and this provides the
  generalization to comparing two sets rather than just a pair of predictors.

  The computed ratio is the square root of the variance-ratio quantity
  denoted as `omega' in Silber, J H, Rosenbaum, P R and Ross, R N
  (1995).  Estimated standard errors are calculated by
  the delta method, as described in that paper for example. 
  
  If \code{set1} and \code{set2} are unspecified, and if the \code{tcltk} package has been 
  loaded, a dialog box is provided (by a call to \code{\link{pickFrom}})
  for the choice of \code{set1} and \code{set2} from the available model coefficients.
}

\value{
  An object of class \code{relimp}, with at least the following components:
  \item{model}{The call used to construct the model object summarized}
  \item{sets}{The two sets of indices specified as arguments}
  \item{log.ratio}{The natural logarithm of the ratio of effect
    standard deviations corresponding to the two sets specified}
  \item{se.log.ratio}{An estimated standard error for log.ratio}
  
  If \code{dispersion} was supplied as an argument, its value is stored as the
  \code{dispersion} component of the resultant object.
}
\references{Silber, J. H., Rosenbaum, P. R. and Ross, R N (1995)
  Comparing the Contributions of Groups of Predictors: Which Outcomes
  Vary with Hospital Rather than Patient Characteristics?  \emph{JASA} \bold{90},
  7--18.
  }
\author{David Firth \email{d.firth@warwick.ac.uk} }

\seealso{\code{\link{relrelimp}}}

\examples{
x <- rnorm(100)
z <- rnorm(100)
w <- rnorm(100)
y <- 3+ 2*x + z + w + rnorm(100)
test <- lm(y ~ x +z +w)
print(test)
relimp(test, 2, 3)    #  compares effects of x and z
relimp(test, 2, 3:4)  #  compares effect of x with that of (z,w) combined
##
##  Data on housing and satisfaction, from Venables and Ripley
##  -- multinomial logit model
library(MASS)
library(nnet)
data(housing)
house.mult <- multinom(Sat ~ Infl + Type + Cont, weights = Freq,
  data = housing)
relimp(house.mult, set1 = 2:3, set2 = 7, response.cat = "High")
}
\keyword{models}
\keyword{regression}
