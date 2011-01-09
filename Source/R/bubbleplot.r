list2table3l = function(l1, l2, l3){
	numbers = c(length(intersect(setdiff(l1, l2), setdiff(l1, l3))),
	            length(intersect(setdiff(l2, l1), setdiff(l2, l3))),
	            length(intersect(setdiff(l3, l1), setdiff(l3, l2))),
							length(setdiff(intersect(l1, l2), l3)),
							length(setdiff(intersect(l1, l3), l2)),
							length(setdiff(intersect(l3, l2), l1)),
							length(intersect(intersect(l1, l2), l3)))
	percentage = round(numbers / sum(numbers), 2) * 100
	return(data.frame(numbers = numbers, percentage = percentage))
}

list2table2l = function(l1, l2){
	numbers = c(length(setdiff(l1, l2)), length(intersect(l1, l2)), length(setdiff(l2, l1)))
	percentage = round(numbers / sum(numbers), 2) * 100
	return(data.frame(numbers = numbers, percentage = percentage))
}

scale_radius = function(data, const = 0.25){
	data$radius = sqrt(data$percentage)/10 * const
	return(data)
}

bubbleplot2l = function(lists, percentage = TRUE){
	x = c(0.25, 0.5, 0.75)
	y = c(0.5, 0.5, 0.5)
	data = list2table2l(lists[[1]], lists[[2]])
	data = data.frame(x, y, data)
	data = scale_radius(data, const = 0.2)
	data = data[order(data$numbers, decreasing = TRUE), ]
	
	grid.newpage()
	grid.lines(x = data$x, y = data$y, gp = gpar(col = "grey"))
	grid.circle(x = data$x, y = data$y, r = data$radius, gp = gpar(fill = c("#E41A1C", "#377EB8", "#4DAF4A"), col = "white"))
	if(percentage){
		grid.text(label = data$percentage, x = data$x, y = data$y)
	}
	else{
		grid.text(label = data$numbers, x = data$x, y = data$y)
	}
	
	grid.text(label = names(lists), x = x[c(1, 3)], y = y[c(1, 3)] + max(data$radius) * 1.05, vjust = 0, hjust = c(0.5, 0.5), gp = gpar(cex = 1.5))
}

bubbleplot3l = function(lists, percentage = TRUE){
	data = list2table3l(lists[[1]], lists[[2]], lists[[3]])
	x = c(0.25, 0.5, 0.75, 0.375, 0.5, 0.625, 0.5)
	y = c(0.25, 0.68, 0.25, 0.47, 0.25, 0.47, 0.39)
	data = data.frame(x, y, data)
	data = scale_radius(data, const = 0.2)
	triangle =  data[c(1, 2, 3, 1, 7, 3, 2, 7), c("x", "y")]
	data = data[order(data$numbers, decreasing = TRUE), ]
	
	grid.newpage()
	grid.lines(x = triangle$x, y = triangle$y, gp = gpar(col = "grey60"))
	grid.circle(x = data$x, y = data$y, r = data$radius, gp = gpar(fill = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628"), col = 0))
	if(percentage){
		grid.text(label = data$percentage, x = data$x, y = data$y)
	}
	else{
		grid.text(label = data$numbers, x = data$x, y = data$y)
	}
	grid.text(label = names(lists), x = x[c(1:3)], y = y[c(1:3)] - c(1, -1, 1) * max(data$radius) * 1.05, vjust = c(1, 0, 1), gp = gpar(cex = 1.5))
}


 
#' Function to draw the bubbleplots - an alternative to Venn diagram
#' 
#' Function to draw the bubbleplots - an alternative to Venn diagram
#'
#' @param lists input element lists
#' @param percentage logical showing if percentages or raw numbers are displayed
#' @return  Returns nothing only draws the picture
#' @author  Raivo Kolde <rkolde@@gmail.com>
#' @examples
#' bubbleplot(list(Asadsa = sample(letters, 14), Gadsa = sample(letters, 9)))
#' bubbleplot(list(Badsad = sample(letters, 16), Asadsa = sample(letters, 14), Gadsa = sample(letters, 9)))
#' 
#' @export
bubbleplot = function(lists, percentage = TRUE){
	n = length(lists)
	if(n < 2){
		stop("Please provide at least 2 lists")
	}
	if(n == 2){
		bubbleplot2l(lists, percentage = TRUE)
	}
	if(n > 2){
		bubbleplot3l(lists[1:3], percentage = TRUE)
	}
}








