\name{gconvert}
\alias{gconvert}
\title{Convert gene ID-s.}
\usage{gconvert(ids, organism="hsapiens", target="ENSG", df=T)
}
\description{Convert gene ID-s.}
\details{Wrapper for g:Convert web toolkit.}
\value{The output can be either list or data.frame. List has an entry for every input gene. Data frame is just a two column table with inputs and corrsponding outputs. The input names may be duplicated.}
\references{J. Reimand, M. Kull, H. Peterson, J. Hansen, J. Vilo: g:Profiler - a web-based toolset for functional profiling of gene lists from large-scale experiments (2007) NAR 35 W193-W200}
\author{Juri Reimand <jyri.reimand@ut.ee>, Raivo Kolde <rkolde@gmail.com>}
\arguments{\item{ids}{gene list.}
\item{organism}{gene list origin.}
\item{target}{target ID.}
\item{df}{logical showing if the output will be a data.frame or list.}
}
\examples{gconvert(c("Klf4", "Pax5", "Sox2", "Nanog"), organism = "mmusculus")}

