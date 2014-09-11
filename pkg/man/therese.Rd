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
Version: \tab 0.3\cr
Date: \tab 2014-09-11\cr
License: \tab GPL (>= 2)\cr
Repository: \tab R-Forge
}

The name 'therese' has been chosen in reference to Julien Chiquet's R package
'simone'. If you really want to know what it stands for, it's Trust the Holy 
Estimation of Regulatory NEtworks from Several Expression data.
}
\author{
Nathalie Villa-Vialaneix <nathalie.villa@toulouse.inra.fr>

Nicolas A. Edwards <nicolas.ae@free.fr>

Maintainer: Nathalie Villa-Vialaneix
}
\references{
Villa-Vialaneix N., Vignes M., Viguerie N., San Cristobal M. (2014) Inferring
networks from multiple samples with consensus LASSO. \emph{Quality Technology
and Quantitative Management}, \bold{11}(1), 39--60.
}
\seealso{
\code{\link{build.therese}}, \code{\link{therese2igraph}}
}
\examples{
data(cancer)
\dontrun{build.therese(expr, conditions, mu=1)}
}
