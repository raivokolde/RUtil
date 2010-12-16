#' Find orthologs.
#' 
#' Wrapper for g:Orth web toolkit. The organism names are constructed, by combining the first letter of 
#' the name and family name. For example human - 'hsapiens' and mouse- 'mmusculus' 
#' 
#' To alleviate the problem of having many orthologs per gene (most of 
#' them uninformative) one can set a threshold of how many results to show. The programs tries to find the 
#' most informative by selecting the most popular ones. 
#'
#' @param genelist list of gene names to be translated.
#' @param source_organism organism of the input genes.
#' @param target_organism name for the target organism. 
#' @param mthreshold how many ortholog names per gene to show
#' @param df logical showing if the output will be a data.frame or list. 
#' @return  The output can be either list or data.frame. List has an entry for every input gene. Data 
#' frame is just a two column table with inputs and corrsponding outputs. The input names may be 
#' duplicated.
#' @references  J. Reimand, M. Kull, H. Peterson, J. Hansen, J. Vilo: g:Profiler -- a web-based toolset for functional profiling of gene lists from large-scale experiments (2007) NAR 35 W193-W200 
#' @author  Raivo Kolde <rkolde@@gmail.com>, Juri Reimand <jyri.reimand@@ut.ee>
#' @examples
#'  gorth(c("Klf4", "Pax5", "Sox2", "Nanog"), source_organism = "mmusculus", target_organism = "hsapiens")
#' @export
gorth <- function(genelist, source_organism = "mmusculus", target_organism = "hsapiens", mthreshold = 3, df = T){
	my_url <- "http://biit.cs.ut.ee/gprofiler/gorth.cgi"
	
	if(length(genelist) == 0) return(NULL)
	
	raw_query <- postForm(my_url, 
		output = 'mini', 
		query = str_c(genelist, collapse = " "), 
		organism = source_organism,
		target = target_organism
	)
	
	conn <- textConnection(raw_query)
	tab <- read.table(conn, sep = "\t")
	close(conn)
	
	colnames(tab)[2] <- "Source"
	
	res = dlply(tab, "Source", function(x){
		
		x = x[x$V6 != "N/A",]
		if(nrow(x) == 0){
			return(NULL)
		}
		
		if(length(x$V6) < mthreshold + 1){
			return(as.character(x$V6))
		} 
		else{
			return(names(rev(sort(table(as.character(x$V6)))))[1:mthreshold])
		}
	})
	
	if(df){
		res <- ldply(res, function(x) data.frame(Target = x))
	}
	
	return(res)
}
 
#' Annotate gene list functionally.
#' 
#' Wrapper for g:Profiler web toolkit for finding enrichments in gene lists.
#'
#' @param organism gene list origin 
#' @param query vector of gene names or list of such vectors
#' @param ordered_query when output gene lists are ranked one can use this option to get GSEA style 
#' p-values.
#' @param significant logical indicating if all or only statistically significant results should be 
#' returned.
#' @return Data frame with the Enricment analysis results. If input consisted of several lists the corresponding list is indicated with a variable 'query number'
#' @references  J. Reimand, M. Kull, H. Peterson, J. Hansen, J. Vilo: g:Profiler - a web-based toolset for functional profiling of gene lists from large-scale experiments (2007) NAR 35 W193-W200
#' @author  Juri Reimand <jyri.reimand@@ut.ee>
#' @examples
#'  gprofiler(c("Klf4", "Pax5", "Sox2", "Nanog"), organism = "mmusculus")
#' @export
gprofiler <- function(organism='scerevisiae', query, ordered_query=0, significant=1) {
	
	query_url = ""

	if (is.list(query)) {
		for (i in 1:length(query)) {
			query_url <- paste(sep="\n", query_url, paste("> query", i), paste(query[[i]], collapse=" "))
		}
	} else if (is.vector(query)) {
		query_url <- paste(query, collapse=" ")
	} else {
		print("ERROR missing query")
		return()
	}
	
	my_url <- "http://biit.cs.ut.ee/gprofiler/gcocoa.cgi"
	
	raw_query <- postForm(my_url, 
		organism=organism, 
		output='mini', 
		query=query_url, 
		analytical="1", 
		sort_by_structure="1", 
		significant=as.character(significant), 
		ordered_query=as.character(ordered_query)
	)

	split_query <- unlist(strsplit(raw_query, split="\n"))
	
	commented_lines <- grep("^#", split_query)
	if (length(commented_lines)>0) {
		split_query <- split_query[-commented_lines]
	}
	empty_lines <- which(split_query == "")
	if (length(empty_lines)>0) {
		split_query <- split_query[-empty_lines]
	}

	split_query <- t(as.data.frame(strsplit(split="\t", split_query)))

	if (dim(split_query)[1] == 0) {
			split_query <- matrix(NA, 0, 13)
	}
	split_query[,12] <- gsub("^\\s+", "", split_query[,12])
	
	split_query[,2][which(split_query[,2] == "")] <- 0
	split_query[,2][which(split_query[,2] == "!")] <- 1
	
	rownames(split_query) <- NULL
	colnames(split_query) <- c(
		"query.number", "significant", "p.value", 
		"term.size", "query.size", "overlap.size", 
		"precision", "recall", "term.id", 
		"domain", "subgraph.number", "term.name", "relative.depth")
	
	split_query <- as.data.frame(split_query, colClasses=c("numeric"))
	split_query[,3] <- as.numeric(gsub("e","E",split_query[,3]))
	
	if(is.list(query) & !is.null(names(query))){
		split_query$query.number = names(query)[split_query$query.number]  
	}
	
	return(split_query)
}

 
#' Convert gene ID-s.
#' 
#' Wrapper for g:Convert web toolkit.
#'
#' @param ids gene list.
#' @param organism gene list origin.
#' @param target target ID.
#' @param df logical showing if the output will be a data.frame or list.
#' @return The output can be either list or data.frame. List has an entry for every input gene. Data frame is just a two column table with inputs and corrsponding outputs. The input names may be duplicated.
#' @references  J. Reimand, M. Kull, H. Peterson, J. Hansen, J. Vilo: g:Profiler - a web-based toolset for functional profiling of gene lists from large-scale experiments (2007) NAR 35 W193-W200
#' @author  Juri Reimand <jyri.reimand@@ut.ee>, Raivo Kolde <rkolde@@gmail.com>
#' @examples
#'  gconvert(c("Klf4", "Pax5", "Sox2", "Nanog"), organism = "mmusculus")
#' @export
gconvert = function(ids, organism="hsapiens", target="ENSG", df = T) {
	
	url = "http://biit.cs.ut.ee/gprofiler/gconvert.cgi"
	
	raw_query = postForm(url, 
		organism=organism, 
		output='mini', 
		query=paste(ids, collapse="+"), 
		target=target
	)
	
	split_query = unlist(strsplit(raw_query, split="\n"))
	
	commented_lines = grep("^#", split_query)
	if (length(commented_lines)>0) {
		split_query = split_query[-commented_lines]
	}
	empty_lines = which(split_query == "")
	if (length(empty_lines)>0) {
		split_query = split_query[-empty_lines]
	}

	split_query = t(as.data.frame(strsplit(split="\t", split_query)))
	rownames(split_query) = c()

	resulting_mapping = list()
	for (id in unique(toupper(ids))) {
		my_map = split_query[split_query[,2]==id,,drop=F][,4]
		my_map = my_map[my_map != "N/A"]
		resulting_mapping[[id]] = my_map
	}
	
	if(df){
		resulting_mapping <- ldply(resulting_mapping, function(x) data.frame(Target = x))
	}
	
	return(resulting_mapping)
}

 
#' Generate query for g:Cocoa web tool
#' 
#' Function that prints out a g:Gogoa query string given a list of gene name vectors.
#'
#' @param glist a list of vectors of gene names			
#' @return  Prints out the query string
#' @author  Raivo Kolde <rkolde@@gmail.com>
#' @examples
#'  glist = list(a = c("pax6", "klf9"), b = c("nanog", "Pou5f1"))
#' @export
generate_gcocoa_query = function(glist){
	if(is.null(names(glist))){
		names(glist) = paste("X", 1:length(glist), sep = "")
	}
	for(i in names(glist)){
		cat(sprintf(">%s\n", i))
		cat(int[[i]])
		cat("\n\n")
	}
}


