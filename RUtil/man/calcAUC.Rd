\name{calcAUC}
\alias{calcAUC}
\title{Calculate AUC score based on the given curves...}
\usage{calcAUC(curves, bootstrap=0)
}
\description{Calculate AUC score based on the given curves}
\details{Calculate AUC score based on the given curves}
\value{returns a data frame with auc values for each class, optionally also with 
bootstrapped confidence intervals}
\author{Raivo Kolde <rkolde@gmail.com>}
\arguments{\item{curves}{curves as calculated by \code{\link{calcCurve}}}
\item{bootstrap}{number of bootstrap samples for finding confidence intervals}
}

