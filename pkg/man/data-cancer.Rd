\name{cancer}
\alias{cancer}
\alias{conditions}
\alias{expr}

\docType{data}

\title{Microarray data set for breast cancer} 

\usage{data(cancer)}

\description{%
  This gene expression data set is freely available, coming from the Hess
  \emph{et al}'s paper and also provided in Julien Chiquet's (former?) R package
  'simone'. The original data contains 26 gene expression measures for 133
  individuals classified as either a pathologic complete response (pCR) or
  residual disease (not-pCR).
  
  Here we considered only a subsample of 40 individuals, with balanced
  distribution among the two conditions (because 'therese' is yet not well set
  to deal with unbalanced conditions).
}

\format{%
  Two objects:
  \describe{
    \item{\code{expr}}{a \code{data.frame} with 26 columns and 40 rows rows
    containing the gene expression.
    }
    \item{\code{conditions}}{a factor with 2 levels (\code{"pcr"} and \code{"not"}),
    describing the status of the patient.
    }
  }
}

\references{% 
  Hess K.R., \emph{et al.} (2006) Pharmacogenomic predictor of sensitivity to
  preoperative chemotherapy with Paclitaxel and Fluorouracil, Doxorubicin, and
  Cyclophosphamide in breast cancer. \emph{Journal of Clinical Oncology},
  \strong{24}(26), 4236--4244.
}

\examples{
## load the breast cancer data set
data(cancer)

table(conditions)
\dontrun{boxplot(expr)}
}

\keyword{datasets}
