\name{therese2igraph}
\alias{therese2igraph}
\alias{print.thereseNet}
\alias{summary.thereseNet}
\alias{plot.thereseNet}

\title{
Build a graph from a 'therese' object
}
\description{
This function builds an igraph object from a therese object
}
\usage{
therese2igraph(therese.res, choice=c("BIC","AIC","loglik","last"), ind=NULL)
\method{print}{thereseNet}(x, \ldots)
\method{summary}{thereseNet}(object, \ldots)
\method{plot}{thereseNet}(x, ..., best.layout="indiv")
}

\arguments{
  \item{therese.res}{
an object of type 'therese' as produced by the function
\code{\link{build.therese}}
}
  \item{choice}{
A criterion indicating which result of the therese list has to be considered.
Possible values are \code{BIC} which selects the networks with the best BIC,
\code{AIC} which selects the network with the best AIC, \code{loglik} that
selects the network with the best penalized log-likelihood and \code{last}
which select the last network of the list. This option is not used if the 
argument \code{ind} is specified. Default value is \code{BIC}.
}
  \item{ind}{(optional) index that indicates which networks in the therese list
has to be extracted. It correspond to the \code{index}-th value of the 
\eqn{lambda}{\lambda} used during the training.
}
	\item{x}{
an object of class \code{thereseNet}
  }
  \item{object}{an object of class \code{thereseNet}}
  \item{...}{further argument to be passed to the 
  \code{\link[igraph]{plot.igraph}}} function.
  \item{best.layout}{either equal to \code{indiv} and then all networks will be
displayed with a different layout of their nodes or, alternatively equal to the
index (between 1 and the number of conditions) of the network that will serve
as a reference to generate a layout with the \code{\link[igraph]{layout.auto}}
function.}
}
\details{
If no further arguments are passed to the function
\code{\link{plot.thereseNet}}, the networks will be displayed with blue edges 
corresponding to shared edges between conditions and with pink edges
corresponding to condition specific edges. The edges width will be proportional
to the partial correlation square root and the edges type will correspond to the
sign of the partial correlation (dotted lines are used for negative partial
correlations).
}
\value{
Returns an object of class \code{thereseNet} which is a list of
\code{\link[igraph]{igraph}} objects having for length the number of conditions.
}
\author{
Nicolas A. Edwards <nicolas.ae@free.fr>

Nathalie Villa-Vialaneix <nathalie.villa@univ-paris1.fr>
}
\examples{
data(cancer)
\dontrun{
res <- build.therese(expr, conditions, mu=1)
thereseNets <- therese2igraph(res.therese, "BIC")
summary(therese.nets)
plot(therese.nets)}
}
\keyword{aplot}