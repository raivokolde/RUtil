\name{plotROC}
\alias{plotROC}
\title{Plot ROC curve...}
\usage{plotROC(data, bootstrap=100, colours=NA, text_size=4, all_replicates=TRUE)
}
\description{Plot ROC curve}
\details{Input data frame has to contain two columns Probability and Class. Probability refers 
to some score accordind to which the data is ordered and Class referes to if the 
observation was positive or negative. If several lines are needed, additional column 
algorithm should be provided. The ROC curves and AUC-s are calculated by functions 
\code{\link{calcCurve}} and \code{\link{calcAUC}}. By default the AUC values get also 
confidence intervals, which are calculated using bootstrap. The number of bootstrap 
samples taken can be set with parameter bootstrap. If desired one can also enter 
replicate experiments to calculate confidence intervals. For that the dataset has to 
contain also Column Replicate.}
\value{Decription of return value}
\author{Raivo Kolde <rkolde@gmail.com>}
\arguments{\item{data}{input data.frame}
\item{colours}{what colours should the lines be}
\item{text_size}{AUC text size}
\item{bootstrap}{number of bootstrap samples}
\item{all_replicates}{logical if we show the repilcates as well}
}
\examples{data = data.frame(Probability = runif(300))
data$Class = rbinom(300, 1, prob = 1 - data$Probability)
#plotROC(data)

data$Algorithm = sample(c("A", "B", "C"), 300, replace = TRUE)
#plotROC(data)

data$Replicate = factor(sample(1:5, nrow(data), replace = TRUE))
#plotROC(data)

data$Replicate = factor(sample(1:5, nrow(data), replace = TRUE))
#plotROC(data)}

