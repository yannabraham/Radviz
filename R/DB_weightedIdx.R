
#' Computation of weighted version of the Davies-Bouldin index. This index serves as a measure of clustering quality of a 2D projection result with known class labels
#' 
#' @param x an object of class Radviz, as returned by \code{\link{do.radviz}}.
#' @return weighted DB index value
#' 
#' @author Nicolas Sauwen
#' @export
DB_weightedIdx <- function(x){
	
	data <- x$data
	springs <- x$springs
	classes <- x$classes
	
	if(is.null(classes)){
		stop("Davies-Bouldin weighted index can only be computed on radviz object of type 'freeviz'")
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
