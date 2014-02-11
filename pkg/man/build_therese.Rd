\name{build.therese}
\alias{build.therese}
\alias{print.therese}
\alias{summary.therese}

\title{
Build network with the Consensus Lasso method
}
\description{
This function builds a network from the consensus Lasso method.
}
\usage{
build.therese(expr, conditions, ...)
\method{print}{therese}(x, \ldots)
\method{summary}{therese}(object, \ldots)
}

\arguments{
  \item{expr}{
an nxp matrix of data.
}
  \item{conditions}{
a vector indicating the condition.
}
  \item{...}{
further arguments to be passed to the function \code{\link{set.options}}.
  }
  \item{x}{
an object of class \code{therese}
  }
  \item{object}{an object of class \code{therese}}
}
\value{
Returns an object of class \code{therese} that contains the following elements:
\item{data}{the original expression data \code{expr}}
\item{conditions}{the original vector of conditions \code{conditions}}
\item{networks}{a list of concentration matrices corresponding to the different
values of the parameter \eqn{lambda}{\lambda} and to the different conditions:

\code{networks[[lambda.n]][[cond.n]]}

corresponds to the concentration matrix of the \code{lambda.n}-th
\eqn{lambda}{\lambda} and to the \code{cond.n}-th condition.}
\item{betas}{a list of the coefficients in the regression. They are sorted as
\code{networks}.}
\item{pcors}{a list of the partial correlations. They are sorted as
\code{networks}.}
\item{used.lambdas}{the list of the \eqn{lambda}{\lambda}s used during the
the inference.}
\item{n.edges}{the number of edges of the inferred network for every value of 
\eqn{lambda}{\lambda}. The number of edges is given as a matrix with 
\code{length(nlevels(conditions))} columns and a number of rows equal to the 
number of used \eqn{lambda}{\lambda}.}
\item{BIC}{the vector of BIC values for every value of \eqn{lambda}{\lambda}.}
\item{AIC}{the vector of AIC values for every value of \eqn{lambda}{\lambda}.}
\item{loglik}{a vector of log-likelihood values for every value of
\eqn{lambda}{\lambda}.}
\item{loglik.pen}{the vector of penalized log-likelihood values for every value
of \eqn{lambda}{\lambda}.}
\item{options}{the options of the \code{therese} algorithm as set by
\code{\link{set.options}}}
}
\author{
Nathalie Villa-Vialaneix <nathalie.villa@univ-paris1.fr>
}
\references{
Villa-Vialaneix N., Vignes M., Viguerie N., San Cristobal M. (2014) Inferring
networks from multiple samples with consensus LASSO. \emph{Quality Technology
and Quantitative Management}, \bold{11}(1), 39--60.
}
\seealso{
\code{\link{set.options}}, \code{\link{therese2igraph}}
}
\examples{
data(cancer)
\dontrun{build.therese(expr, conditions, mu=1)}
}
\keyword{methods}