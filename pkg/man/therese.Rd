\name{therese-package}
\alias{therese-package}
\alias{therese}
\docType{package}
\title{
Multiple Network Inference by therese's Consensus Lasso
}
\description{
This package is another package for multiple network inference (*). 
}
\details{
\tabular{ll}{
Package: \tab therese\cr
Type: \tab Package\cr
Version: \tab 0.1\cr
Date: \tab 2013-07-09\cr
License: \tab GPL (>= 2)\cr
Repository: \tab R-Forge
}

The name 'therese' has been chosen in reference to Julien Chiquet's (former?) R
package 'simone'. If you really want to know what it stands for, it's
Trust the Holy Estimation of Regulatory NEtworks from Several Expression data.
}
\author{
Nathalie Villa-Vialaneix <nathalie.villa@univ-paris1.fr>

Nicolas A. Edwards <nicolas.ae@free.fr>

Maintainer: Nathalie Villa-Vialaneix
}
\references{
Villa-Vialaneix N., San Cristobal M. (2013) Consensus LASSO : inference
conjointe de reseaux de genes dans des conditions experimentales multiples.
In Proceedings of: \emph{45e Journees de Statistique}, Toulouse, France, May,
27-31, p.40.  \url{http://papersjds13.sfds.asso.fr/submission_13.pdf}.
}
\seealso{
\code{\link{build.therese}}, \code{\link{therese2igraph}}
}
\examples{
data(cancer)
\dontrun{build.therese(expr, conditions, mu=1)}
}
