 
#' Function for producing all unique combinations of elements from one vector.
#' 
#' The function creates all unique combinations of different values in vector l. 
#'
#' @param l vector of some values
#' @return  A matrix with two columns. Rownames are the element names pasted togeteher and separated by 
#' "_". The rows are ordered alphabetically according to rownames. The two values in each row are also 
#' ordered alphabetically.
#' @references  Ref1
#' @author  Raivo Kolde <rkolde@@gmail.com>
#' @examples
#'  make_unique_comb(letters[1:3])
#' @export
make_unique_comb <- function(l){
	l <- unique(l)
	pairs <- t(combn(l, 2))
	pairs <- t(apply(pairs, 1, sort))
	pairs <- matrix(pairs, ncol = 2)
	rownames(pairs) <- str_c(pairs[, 1], pairs[, 2], sep = "_") 
	pairs <- pairs[order(rownames(pairs)), , drop = F]
	colnames(pairs) = c("X1", "X2")
	return(pairs)
}
