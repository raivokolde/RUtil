 
#' Calculate ROC curves based on ordered class data
#' 
#' Calculate ROC curves based on ordered class data
#'
#' @param data Data as described in \code{\link{data}}
#' @return  Returns a data frame like data, but with additional columns TP and TN
#' @author  Raivo Kolde <rkolde@@gmail.com> 
#' @export
calcCurve = function(data){
	if(!("Algorithm" %in% colnames(data))){
		data$Algorithm = 1
		data$Algorithm = factor(data$Algorithm)
	}
	
	if(!("Replicate" %in% colnames(data))){
		data$Replicate = factor(1)
	}
	
	if(class(data$Class) != "factor"){
		data$Class = factor(data$Class)
	}
	
	res = ddply(data, c("Algorithm", "Replicate"), function(d) {
		d = d[order(d$Probability),]
		d = transform(d, 
			TN = cumsum(Class == levels(Class)[1])/sum(Class == levels(Class)[1]), 
			TP = cumsum(Class == levels(Class)[2])/sum(Class == levels(Class)[2])
		)
		return(d) 
		}
	)
	
	return(res)
}

 
#' Calculate AUC score based on the given curves
#' 
#' Calculate AUC score based on the given curves
#'
#' @param curves curves as calculated by \code{\link{calcCurve}}
#' @param bootstrap number of bootstrap samples for finding confidence intervals
#' @return  returns a data frame with auc values for each class, optionally also with 
#' bootstrapped confidence intervals
#' @author  Raivo Kolde <rkolde@@gmail.com>
#' @export
calcAUC = function(curves, bootstrap = 0){
	auc = ddply(curves, c("Algorithm", "Replicate"), function(d){
		delta = c(0, d$TN[2:nrow(d)] - d$TN[1:(nrow(d) - 1)])
		auc = sum(delta * d$TP)
		data.frame(auc = auc)
	})
	
	auc2 = ddply(auc, c("Algorithm"), function(d){
		data.frame(auc = mean(d$auc))
	})
	
	if(length(unique(curves$Replicate)) > 1){
		qboot = ddply(auc, "Algorithm", function(x){
			q = 1.96 * sd(x$auc)
			return(data.frame(q = q))
		})
		auc2 = merge(auc2, qboot)
	}
	
	if(bootstrap > 1 & length(unique(curves$Replicate)) == 1){
		cat(sprintf("%s\n", "Bootstrapping..."))
		f = function(data){
			samp = ddply(data, "Algorithm", function(x) x[sample(1:nrow(x), replace = TRUE), ])
			res = calcAUC(calcCurve(samp))
		}
		
		boot = rdply(bootstrap, f(curves), .progress = "text")
		qboot = ddply(boot, "Algorithm", function(x){
			q = 1.96 * sd(x$auc)
			return(data.frame(q = q))
		})
		auc2 = merge(auc2, qboot)
	}
	return(auc2)
}
# 
# calcAUC(curve, bootstrap = 1000)

 

 
#' Plot ROC curve
#' 
#' Input data frame has to contain two columns Probability and Class. Probability refers 
#' to some score accordind to which the data is ordered and Class referes to if the 
#' observation was positive or negative. If several lines are needed, additional column 
#' algorithm should be provided. The ROC curves and AUC-s are calculated by functions 
#' \code{\link{calcCurve}} and \code{\link{calcAUC}}. By default the AUC values get also 
#' confidence intervals, which are calculated using bootstrap. The number of bootstrap 
#' samples taken can be set with parameter bootstrap. If desired one can also enter 
#' replicate experiments to calculate confidence intervals. For that the dataset has to 
#' contain also Column Replicate.
#' 
#'
#' @param data input data.frame
#' @param colours what colours should the lines be
#' @param text_size AUC text size
#' @param bootstrap number of bootstrap samples
#' @param all_replicates logical if we show the repilcates as well
#' @return  Decription of return value
#' @author  Raivo Kolde <rkolde@@gmail.com>
#' @examples
#'  data = data.frame(Probability = runif(300))
#'  data$Class = rbinom(300, 1, prob = 1 - data$Probability)
#'  #plotROC(data)
#' 
#'  data$Algorithm = sample(c("A", "B", "C"), 300, replace = TRUE)
#'  #plotROC(data)
#' 
#'  data$Replicate = factor(sample(1:5, nrow(data), replace = TRUE))
#'  #plotROC(data)
#' 
#' data$Replicate = factor(sample(1:5, nrow(data), replace = TRUE))
#' #plotROC(data)
#' 
#' @export
plotROC = function(data, bootstrap = 100, colours = NA, text_size = 4, all_replicates = TRUE){
	curve = calcCurve(data)
	auc = calcAUC(curve, bootstrap = bootstrap)
	
	if(ncol(auc) > 2){
		auc = paste(sprintf("AUC(%s) = %.3f (+/- %.3f)", auc$Algorithm, auc$auc, auc[, 3]), collapse = "\n")
	}
	else{
		auc = paste(sprintf("AUC(%s) = %.3f", auc$Algorithm, auc$auc), collapse = "\n")
	}
	
	data2 = data
	data2$Replicate = 1
	average = calcCurve(data2)
	
	p = qplot(x = TN, y = TP, geom = "line", linetype = Algorithm, alpha = I(1), colour = Algorithm, data = average)	+ annotate(geom = "text", x = 1, y = 0, label = auc, colour = "grey15", size = text_size, hjust = 1, vjust = 0) + theme_bw()
	if(!is.na(colours[1])){
		p = p + scale_colour_manual(values = colours)
	}
	
	if(length(unique(data$Replicate)) > 1 & all_replicates){
		p = p + geom_line(aes(x = TN, y = TP, group = interaction(Replicate, Algorithm), linetype = Algorithm, colour = Algorithm), alpha = 0.5, data = curve)
	}
	return(p)
}
# plotROC(data)

# 
# # 
# data = data.frame(Probability = runif(3000))
# data$Class = rbinom(nrow(data), 1, prob = 1 - data$Probability)
# plotROC(data)
#  
# data$Algorithm = sample(c("A", "B", "C"), nrow(data), replace = TRUE)
# plotROC(data)
# 
# data$Replicate = factor(sample(1:5, nrow(data), replace = TRUE))
# plotROC(subset(data, Replicate %in% 1:5))



# 
# 
# plotROC(res)