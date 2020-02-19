
#' Computation of weighted version of the Davies-Bouldin index. This index serves as a measure of clustering quality of a 2D projection result with known class labels
#' 
#' @param x an object of class Radviz, as returned by \code{\link{do.radviz}}.
#' @return weighted DB index value
#' 
#' @author Nicolas Sauwen
#' @export
DB_weightedIdx <- function(x){
	
	if(x$type == "Graphviz"){
		stop("Davies-Bouldin weighted index can not be computed on radviz object of type 'graphviz'")
	}
		
	data <- x$proj$data[1:(ncol(x$proj$data)-3)]
	springs <- x$proj$plot_env$springs
	springNames <- rownames(springs)
	classes <- unlist(data[!(colnames(data) %in% springNames)])
	data <- data[,springNames]
	
	if(length(classes) != nrow(data)){
		stop("Class labels could not be extracted from the radviz object")
	}
	
	classes <- as.integer(as.factor(classes))
	nClasses <- length(unique(classes))
	clusterSizes <- table(classes)
	
	dataNormalized <- apply(data,2, do.L) 
	projectedData <- dataNormalized%*%springs
	
	if(!requireNamespace("clusterSim", quietly = FALSE)) install.packages("clusterSim")	
	DBResult <- clusterSim::index.DB(projectedData, classes)
#	R_vals <- DBResult$r
#	DBIdx <- sum(R_vals*clusterSizes)/sum(clusterSizes)
	meanRVect <- unlist(lapply(1:nClasses, function(k) mean(DBResult$R[k,][-k])))
	DBIdx <- sum(table(classes)*meanRVect)/sum(table(classes))
	
	return(DBIdx)
}
