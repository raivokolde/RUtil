\name{bubbleplot}
\alias{bubbleplot}
\title{Function to draw the bubbleplots - an alternative to Venn diagram...}
\usage{bubbleplot(lists, percentage=TRUE)
}
\description{Function to draw the bubbleplots - an alternative to Venn diagram}
\details{Function to draw the bubbleplots - an alternative to Venn diagram}
\value{Returns nothing only draws the picture}
\author{Raivo Kolde <rkolde@gmail.com>}
\arguments{\item{lists}{input element lists}
\item{percentage}{logical showing if percentages or raw numbers are displayed}
}
\examples{bubbleplot(list(Asadsa = sample(letters, 14), Gadsa = sample(letters, 9)))
bubbleplot(list(Badsad = sample(letters, 16), Asadsa = sample(letters, 14), Gadsa = sample(letters, 9)))}

