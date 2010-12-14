\name{gorth}
\alias{gorth}
\title{Find orthologs.}
\usage{gorth(genelist, source_organism="mmusculus", target_organism="hsapiens", mthreshold=3, df=T)
}
\description{Find orthologs.}
\details{Wrapper for g:Orth web toolkit. The organism names are constructed, by combining the first letter of 
the name and family name. For example human - 'hsapiens' and mouse- 'mmusculus' 

To alleviate the problem of having many orthologs per gene (most of 
them uninformative) one can set a threshold of how many results to show. The programs tries to find the 
most informative by selecting the most popular ones.}
\value{The output can be either list or data.frame. List has an entry for every input gene. Data 
frame is just a two column table with inputs and corrsponding outputs. The input names may be 
duplicated.}
\references{J. Reimand, M. Kull, H. Peterson, J. Hansen, J. Vilo: g:Profiler -- a web-based toolset for functional profiling of gene lists from large-scale experiments (2007) NAR 35 W193-W200}
\author{Raivo Kolde <rkolde@gmail.com>}
\arguments{\item{genelist}{list of gene names to be translated.}
\item{source_organism}{organism of the input genes.}
\item{target_organism}{name for the target organism.}
\item{mthreshold}{how many ortholog names per gene to show}
\item{df}{logical showing if the output will be a data.frame or list.}
}
\examples{gorth(c("Klf4", "Pax5", "Sox2", "Nanog"), source_organism = "mmusculus", target_organism = "hsapiens")}

