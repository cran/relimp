\name{relrelimp}
\alias{relrelimp}

\title{Comparison of Relative Importances in a 
     Multinomial Logit Model}
\description{
     Produces a summary
     of the relative importance of two predictors or two sets of predictors
     in a fitted \code{\link{multinom}} model object, and compares 
     relative importances
     across two of the fitted logit models.
}
\usage{
relrelimp(object, set1=NULL, set2=NULL, label1="set1", label2="set2", 
          subset=TRUE, 
          response.cat1=NULL, response.cat2=NULL)
}

\arguments{
  \item{object}{A model object of class
    \code{multinom}}
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
  \item{response.cat1}{A character
    string used to specify the first regression of interest
    (i.e., the regression
    which predicts the log odds on \code{response.cat1} versus the model's 
    reference category).  The \code{response.cat1} argument should be an
    element of \code{object$lab}.}
  \item{response.cat2}{A character
    string used to specify the second regression of interest
    (i.e., the regression
    which predicts the log odds on \code{response.cat2} versus the model's 
    reference category).  The \code{response.cat2} argument should be an
    element of \code{object$lab}.}
}

\details{Computes a relative importance summary as described in
  \code{\link{relimp}}, for each of the two regressions specified by 
  \code{response.cat1}
  and \code{response.cat2} (relative to the same
  reference category); and computes the 
  difference of those two relative importance summaries,
  along with an estimated
  standard error for that difference.
}

\value{
  An object of class \code{relrelimp}, with at least the following components:
  \item{model}{The call used to construct the model object summarized}
  \item{sets}{The two sets of indices specified as arguments}
  \item{response.category}{A character vector containing the specified
    \code{response.cat1} and \code{response.cat2}}
  \item{log.ratio}{The natural logarithm of the ratio of effect
    standard deviations corresponding to the two sets specified.
    A vector with 
    three components: the first is for \code{response.cat1}
    versus the reference
    category, the second for \code{response.cat2} versus the
    reference category,
    the third is the difference.}
  \item{se.log.ratio}{Estimated standard errors for the elements of 
    \code{log.ratio}}
}

\author{David Firth, \email{david.firth@nuffield.ox.ac.uk} }
\note{Makes use of the generic accessor function \code{\link{vcov}} from
the \code{MASS} package.}

\seealso{\code{\link{relimp}}}

\examples{
##  Data on housing and satisfaction, from Venables and Ripley
library(MASS)
library(nnet)
data(housing)
house.mult <- multinom(Sat ~ Infl + Type + Cont, weights = Freq,
  data = housing)
relrelimp(house.mult, set1 = 2:3, set2 = 7, 
                      label1 = "Influence", label2 = "Contact",
                      response.cat1 = "Medium", response.cat2 = "High")
## Computes the relative contribution of Influence and Contact in 
## each of the two logistic regressions (Med/Low and High/Low), and
## compares those two relative-contribution measures.
}
\keyword{models}
\keyword{regression}
