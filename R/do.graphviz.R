

#' Projects an attributes Matrix or a Data Frame with corresponding graph to a 2D space using Graphviz algorithm
#' 
#' @param x a data.frame or matrix to be projected, with column names matching row names in springs
#' @param graph \code{igraph} object
#' @param attractG Number specifying the weight of the attractive forces
#' @param repelG Number specifying the weight of the repulsive forces
#' @param law Integer, specifying how forces change with distance: 0 = (inverse) linear, 1 = (inverse) square
#' @param steps Number of iterations of the algorithm before re-considering convergence criterion
#' @param springs Numeric matrix with initial anchor coordinates. When \code{NULL} (=default), springs are initialized by \code{\link{make.S}} 
#' 
#' @return An object of class radviz with the following slots:
#'          \itemize{
#'            \item \code{data} the original data (\code{x})
#'            \item \code{springs} the computed \code{springs}
#'            \item \code{projected} the projection of \code{x} on \code{springs},
#'                    a matrix of 2D coordinates for every line in df
#'            \item \code{valid} a logical vector 
#'  		  \item \code{type} character string, indicating method used for computing the projection (in this case "graphviz")
#'            \item \code{graph} the original graph \code{igraph} object
#'          }
#' 
#' @author Nicolas Sauwen
#' @export
do.graphviz <- function(x, graph, attractG = 1, repelG = 1, law = 0, steps = 10, springs = NULL){
	
	if(class(x) ==  "x.frame") x <- as.matrix(x)
	if(!requireNamespace("igraph", quietly = FALSE)) install.packages("igraph")	
	
	# Get edge info from graph
	edges <- igraph::E(graph)
	edgesMat <- igraph::ends(graph, edges, names = FALSE)
	mode(edgesMat) <- "integer"
	edgeWeights <- edges$weight
	
	rm(list = c("edges"))
	
	edgesMat <- rbind(edgesMat, edgesMat[,2:1])
	edgeWeights <- c(edgeWeights, edgeWeights)
	orderInd <- order(edgesMat[,1])
	edgesMat <- edgesMat[orderInd,]
	edgeWeights <- edgeWeights[orderInd]
	degreeVect <- degree(graph)
	edgesInds <- edgesMat[,2]
	
	rm(list = c("edgesMat", "orderInd"))
	
	if(is.null(springs)) springs <-  make.S(colnames(x))
	dataNormalized <- apply(x,2, do.L) 
	dataNormalized <- t(dataNormalized) # C++ code currently assumes observations as columns and attributes as rows
	graphVizSprings <- springs
	
	maxIters <- 1e5
	iter <- 1
	converged <- FALSE
	convTol <- 1e-3
	
	while(!converged & iter < maxIters){
		
		oldSprings <- graphVizSprings
		graphVizSprings <- optimizeAnchorsGraph(dataNormalized, edgesInds, edgeWeights, degreeVect, graphVizSprings, attractG, repelG, law, steps, normalizeExamples = 0)
		
		# Avoid axes being suppressed in the first convergence steps:
		if(iter < 5){
			axesLengths <- sqrt(apply(graphVizSprings^2, 1, sum))
			suppressedInds <- which(axesLengths < 1e-2)
			if(length(suppressedInds) > 0){
				graphVizSprings <-  pracma::flipdim(springs, 1)
			}
		}
		
		springsDiff <- sqrt(apply((oldSprings - graphVizSprings)^2, 1, sum))
		if(max(springsDiff) < convTol) converged <- TRUE
		iter <- iter + 1
	}
	
	rownames(graphVizSprings) <- rownames(springs)
#	colnames(graphVizSprings) <- c("x","y")
	
	print(paste0("# iters: ", iter))

	if(iter == maxIters) warning("Maximum number of iterations reached without convergence")
	
# Create Radviz object
	radvizObject <- do.radviz(as.data.frame(x), graphVizSprings, type = "graphviz", graph = graph)
	
	return(radvizObject)
	
}



