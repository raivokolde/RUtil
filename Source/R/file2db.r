 
#' Create SQLite database from textfile
#' 
#' Function wraps sqldf package to create a database from a text file.
#'
#' @param file text file with the data
#' @param dbname name of the database file 
#' @param title title that the table will have in the database
#' @param ... parameters reminicent from read.table (header, sep, dec, ...) for parsing 
#' the file
#' @author  Raivo Kolde <rkolde@@gmail.com> 
#' @export
file2db = function(file, dbname, title = "f", ...){
	assign(title, file(file))
	
	sqldf(dbname = dbname)
	sqldf(sprintf("select * from %s limit 5", title), dbname = dbname, file.format = list(...))
	connection = getOption("sqldf.connection")
	options(sqldf.connection = NULL)
}
