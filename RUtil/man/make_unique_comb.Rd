\name{make_unique_comb}
\alias{make_unique_comb}
\title{Function for producing all unique combinations of elements from one vector.}
\usage{make_unique_comb(l)
}
\description{Function for producing all unique combinations of elements from one vector.}
\details{The function creates all unique combinations of different values in vector l.}
\value{A matrix with two columns. Rownames are the element names pasted togeteher and separated by 
"_". The rows are ordered alphabetically according to rownames. The two values in each row are also 
ordered alphabetically.}
\author{Raivo Kolde <rkolde@gmail.com>}
\arguments{\item{l}{vector of some values}
}
\examples{make_unique_comb(letters[1:3])}

