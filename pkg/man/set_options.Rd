\name{set.options}
\alias{set.options}
\alias{print.thereseOptions}

\title{
Low-level options for therese inference
}
\description{
This function designs low-level options for therese by setting up various
parameters of the underlying algorithm.
}
\usage{
set.options(scale=TRUE, verbose=TRUE, mu=NULL, penalties=NULL, penalty.min=0.01,
            penalty.max=NULL, n.penalties=100, edges.max=Inf,
            symmetrization=c("AND","OR"), initial.guess=NULL, max.it=NULL)
\method{print}{thereseOptions}(x, \ldots)
}
\arguments{
  \item{scale}{logical specifying wether the data should be normalized to unit
variance. The normalization is made task-wisely. Default is \option{TRUE}.
}
  \item{verbose}{logical indicating the verbose mode that displays the
progression. Default is \option{TRUE}.
}
	\item{mu}{a numeric value penalty for the \eqn{L2}{L^2} consensus penalty. If
\code{NULL} (the default), a realistic value is proposed to the user.
}
  \item{penalties}{a vector of penalties \eqn{lambda}{\lambda} for the network
estimation. These will be used in decreasing order. If \option{NULL} (the
default), an appropriate vector will be generated with \option{n.penalties}
penalties from \option{penalty.max} up to \option{penalty.min} byt the function
\code{\link{build.therese}}.
}
  \item{penalty.min}{the minimal value of the penalty that will be tried for
network inference. Default value is 1e-2. Used only when \option{penalties}
is not supplied.
}
  \item{penalty.max}{the maximal value of the penalty that will be tried for
network inference. If \option{NULL} (the default), it will be set to a value
that corresponds to an empty graph. Used only when \option{penalties} is not
supplied.
}
  \item{n.penalties}{the number of penalties \eqn{lambda}{\lambda} in the
penalties vector. Used only when \option{penalties} is not supplied. Default is
100.
}
  \item{edges.max}{an integer giving an upper bound for the number of edges to
select: if a network is inferred along the path of \eqn{lambda}{\lambda} values
that has a number of edges larger than \option{edges.max}, the algorithm will 
be stopped. Default is \option{Inf}.
}
	\item{initial.guess}{a matrix of the initial guess for the coefficient in the
regression. Its dimensions must be equal to \eqn{(p-1)*cond}{%
(p-1)\times \textrm{cond}} where\eqn{p}{p} is the number of variables (genes)
and \eqn{cond}{\textrm{cond}} is the number of conditions.Default is
\option{NULL}.
}
  \item{symmetrization}{one of \option{AND} or \option{OR} that indicates
which post-symmetrization rule must be applied. Default is \option{AND}.}
	\item{max.it}{maximal number of iteration in the optimization algorithm. If 
\option{NULL} (the default), a realistic value is supplied by the training
algorithm \code{\link{build.therese}} that depends on the sample size and on the
number of variables and conditions.}
  \item{x}{an object of class \code{thereseOptions}}
  \item{...}{not used}
}
\value{
An object of class \code{thereseOptions} which is a list with all the
abovementionned elements.
}
\author{
Nathalie Villa-Vialaneix <nathalie.villa@univ-paris1.fr>
}
\seealso{
\code{\link{build.therese}}
}
\examples{
set.options()
}
