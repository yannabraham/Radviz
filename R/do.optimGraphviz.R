#' Optimize the Dimensional Anchors Position using the Graphviz algorithm
#' 
#' Allows to compute the best arrangement of Dimensional Anchors so that
#' visualization efficiency (i.e. maintaining graph structure) is optimized.
#' The Graphviz algorithm is implemented in C++ for optimal computational efficiency.
#' 
#' @param x a data.frame or matrix to be projected, with column names matching row names in springs
#' @param graph \code{igraph} object
#' @param attractG Number specifying the weight of the attractive forces
#' @param repelG Number specifying the weight of the repulsive forces
#' @param law Integer, specifying how forces change with distance: 0 = (inverse) linear, 1 = (inverse) square
#' @param steps Number of iterations of the algorithm before re-considering convergence criterion
#' @param springs Numeric matrix with initial anchor coordinates. When \code{NULL} (=default), springs are initialized by \code{\link{make.S}}
#' @param weight the name of the attribute containing the edge weights to use for optimization
#' 
#' @importFrom utils install.packages
#' @importFrom stats cutree dist hclust
#' @useDynLib Radviz
#' 
#' @details Graphviz is a variant of Freeviz (\code{\link{do.optimFreeviz}}, applicable to a dataset for which a graph structure (i.e. \code{igraph} object) is available.
#' Attractive forces are defined between connected nodes in the graph, and repulsive forces between all non-connected nodes.
#' To better maintain the original graph structure after projection, spring constants between connected nodes are proportional to their edge weights.
#' Graphviz can be used as an alternative to Freeviz when class labels are not available.
#' 
#' @return A matrix with 2 columns (x and y coordinates of dimensional anchors) and 1 line
#'          per dimensional anchor (so called springs).
#'          
#' @example examples/example-do.radviz.R
#' @examples 
#' 
#' plot(rv,anchors.only=FALSE)
#' 
#' @example examples/example-graphviz.R
#' 
#' @importFrom igraph E ends degree get.edge.attribute
#' @importFrom Rcpp evalCpp
#' 
#' @author Nicolas Sauwen
#' @export
do.optimGraphviz <- function(x, graph, attractG = 1, repelG = 1, 
                             law = 0, steps = 10, springs = NULL,
                             weight = "weight"){
	
	if(any(class(x) ==  "x.frame")) x <- as.matrix(x)
	if(!(law %in% c(0,1))) stop("Parameter 'law' not properly specified. Valid values are 0 or 1")
	if(!requireNamespace("igraph", quietly = FALSE)) install.packages("igraph")	
	
	# Get edge info from graph
	edges <- E(graph)
	edgesMat <- ends(graph, edges, names = FALSE)
	mode(edgesMat) <- "integer"
	edgeWeights <- get.edge.attribute(graph,weight)
	
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
	
	return(graphVizSprings)
	
}







