\name{gprofiler}
\alias{gprofiler}
\title{Annotate gene list functionally.}
\usage{gprofiler(organism="scerevisiae", query, ordered_query=0, significant=1)
}
\description{Annotate gene list functionally.}
\details{Wrapper for g:Profiler web toolkit for finding enrichments in gene lists.}
\value{Data frame with the Enricment analysis results. If input consisted of several lists the corresponding list is indicated with a variable 'query number'}
\references{J. Reimand, M. Kull, H. Peterson, J. Hansen, J. Vilo: g:Profiler - a web-based toolset for functional profiling of gene lists from large-scale experiments (2007) NAR 35 W193-W200}
\author{Juri Reimand <jyri.reimand@ut.ee>}
\arguments{\item{organism}{gene list origin}
\item{query}{vector of gene names or list of such vectors}
\item{ordered_query}{when output gene lists are ranked one can use this option to get GSEA style 
p-values.}
\item{significant}{logical indicating if all or only statistically significant results should be 
returned.}
}
\examples{gprofiler(c("Klf4", "Pax5", "Sox2", "Nanog"), organism = "mmusculus")}

