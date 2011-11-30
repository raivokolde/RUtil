#' Find n closest genomic coordinates in reference for each input coordinate
#' 
#' Function builds a sqlite database from the input and reference and then uses sql to 
#' find the closest points. The input and reference should be given in three column data 
#' frames, first giving the name of the point, second specifying the chromosome and third 
#' the coordinate.
#'
#' @param input point genomic coordinates for input 
#' @param reference point genomic coordinates for output
#' @param n how many closest genes to find
#' @param upstream boolean to indicate, from which side to find the genes
#' @return  Data frame with results
#' @author  Raivo Kolde <rkolde@@gmail.com>
#' @examples
#'  
#' input = data.frame(ID = c("A", "B"), chr = c("chr1", "chr2"), x = c(10, 20))
#' reference = data.frame(ID = letters[1:10], chr = rep(c("chr1", "chr2"), c(5, 5)), x = c(round(runif(10, 1, 30))))
#' find_closest_gpoint(input, reference, n = 3)
#' 
#' @export
find_closest_gpoint = function(input, reference, n = 3, upstream = T){
	colnames(input)[1:3] = c("ID", "chr", "x")
	colnames(reference)[1:3] = c("ID", "chr", "x")
	
	m <- dbDriver("SQLite")
	connection <- dbConnect(m, dbname = ":memory:")
	init_extensions(connection)
	dbWriteTable(connection, '`reference`', reference, row.names = FALSE)
	dbGetQuery(connection, 'create index idx_reference on reference(x)')
	
	# Calculate closest reference points
	res = foreach(i=iter(input, by = "row"), .combine = "rbind") %do% {
		if(upstream){
			r = dbGetQuery(connection, sprintf("SELECT * FROM reference WHERE x > %d AND '%s' = chr ORDER BY x LIMIT %d", i$x, i$chr, n))
		}
		else{
			r = dbGetQuery(connection, sprintf("SELECT * FROM reference WHERE x < %d AND '%s' = chr ORDER BY x LIMIT %d", i$x, i$chr, n))
		}
		if(nrow(r) > 0){
			r$Input_ID = i$ID
			r$Reference_ID = r$ID
			r$Dist = abs(i$x - r$x)
		}
		r
	}
	
	res = res[, c("Input_ID", "Reference_ID", "Dist")]
	
	# Close database connection
	dbDisconnect(connection)
	return(res)
}




 
#' Finds the closest genomic ranges
#' 
#' Given input genomic ranges (data.frame with 4 columns: ID, chromosome, start, end) and 
#' reference ranges (data.frame with 5 columns: ID, chromosome, start, end, strand). Teh 
#' function finds n closest reference ranges from each side to each input range. 
#'
#' @param input_range data frame giving the input ranges (see details)
#' @param reference_range data frame giving the reference ranges (see details)
#' @param n number of results from each side
#' @return  Data frame with results
#' @author  Raivo Kolde <rkolde@@gmail.com>
#' @examples
#'  data(refgene)
#'  find_closest_grange(refgene[sample(1:10000, 10), ], refgene)
#' 
#' @export
find_closest_grange = function(input_range, reference_range, n = 3){
	colnames(input_range)[1:4] = c("Input_ID", "Input_chr", "Input_start", "Input_end")
	colnames(reference_range)[1:5] = c("Reference_ID", "Reference_chr", "Reference_start", "Reference_end", "Reference_strand")
	
	ref = subset(reference_range, Reference_strand == "+")
	plus_up = find_closest_gpoint(input_range[, c("Input_ID", "Input_chr", "Input_end")], ref[, c("Reference_ID", "Reference_chr", "Reference_start", "Reference_strand")], n = 3, upstream = T)
	
	ref = subset(reference_range, Reference_strand == "-")
	minus_down = find_closest_gpoint(input_range[, c("Input_ID", "Input_chr", "Input_start")], ref[, c("Reference_ID", "Reference_chr", "Reference_end", "Reference_strand")], n = 3, upstream = F)
	
	res = rbind(plus_up, minus_down)
	res = merge(res, input_range)
	res = merge(res, reference_range)
	
	ind = c(c("Input_ID", "Input_chr", "Input_start", "Input_end"), c("Reference_ID", "Reference_chr", "Reference_start", "Reference_end", "Reference_strand"), "Dist")
	other = setdiff(colnames(res), ind)
	
	return(res[order(res$Input_ID, res$Reference_start), c(ind, other)])
}


#' A dataset of refegene genes, that can be used as an example for find_closest_grange function. 
#' 
#' @name refgene
#' @docType data
#' @author  Raivo Kolde <rkolde@@gmail.com>
#' @keywords data
NULL
