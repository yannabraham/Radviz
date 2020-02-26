#' Method to compute optimal ratio between repulsive and attractive forces for Freeviz.
#' 
#' @param x Dataframe or matrix, with observations as rows and attributes as columns
#' @param classes Vector with class labels of the observations
#' @param law Integer, specifying how forces change with distance: 0 = (inverse) linear, 1 = (inverse) square
#' @param steps Number of iterations of the algorithm before re-considering convergence criterion
#' @param springs Numeric matrix with initial anchor coordinates. When \code{NULL} (=default), springs are initialized by \code{\link{make.S}}
#' @param multilevel Logical, indicating whether multi-level computation should be used. Setting it to TRUE can speed up computations
#' @param print Logical, indicating whether information on the iterative procedure should be printed in the R console
#' 
#' @details Running Freeviz, it is hard to know what weights to specify for the attractive and repulsive forces to optimize the projection result. This function runs an iterative procedure
#' to find the optimal force ratio. First, a logarithmic grid search is performed, followed by 1D optimization on the refined interval. This approach is less prone to getting stuck in 
#' a suboptimal local optimum, and requires less Freeviz evaluations than direct 1D optimization
#' 
#' @return Value of the optimal force ratio (attractive force in the nominator)
#' 
#' @example examples/example-do.radviz.R
#' @examples
#' plot(rv,anchors.only=FALSE)
#' forceRatio <- tuneForceRatio(x = iris[,das], classes = iris$Species)
#' new.S <- do.optimFreeviz(x = iris[,das], classes = iris$Species, attractG = forceRatio, repelG = 1)
#' new.rv <- do.radviz(iris,new.S)
#' plot(new.rv,anchors.only=FALSE)
#' 
#' @author Nicolas Sauwen
#' @export
tuneForceRatio <- function(x, classes, law = 0, steps = 10, springs = NULL, multilevel = TRUE, print = TRUE){
	
	# Step 1: Logarithmic grid search
	initExponents <- -3:3
	initGrid <- 10^(initExponents)
	initDBVect <- rep(0, length(initGrid))
	
	cat("Start grid search")
	cat("\n")
	
	for(i in 1:length(initGrid)){
		springs <- do.optimFreeviz(x, classes, attractG = initGrid[i], repelG = 1, law = law, steps = steps, springs = springs, multilevel = multilevel, print = print)
		rv <- do.radviz(cbind(x, classes), springs)
		initDBVect[i] <- DB_weightedIdx(rv)
	}
	
	bestInd <- which.min(initDBVect)
	
	# Step 2: 2 more evaluations to further reduce grid
	
	lowerExponent <- initExponents[bestInd] - 0.5
	upperExponent <- initExponents[bestInd] + 0.5
	springs_lower <- do.optimFreeviz(x, classes, attractG = 10^lowerExponent, repelG = 1, law = law, steps = steps, springs = springs, multilevel = multilevel, print = print)
	rv_lower <- do.radviz(cbind(x, classes), springs_lower)
	DB_lower <- DB_weightedIdx(rv_lower)
	springs_upper <- do.optimFreeviz(x, classes, attractG = 10^upperExponent, repelG = 1, law = law, steps = steps, springs = springs, multilevel = multilevel, print = print)
	rv_upper <- do.radviz(cbind(x, classes), springs_upper)
	DB_upper <- DB_weightedIdx(rv_upper)
	
	bestInd2 <- which.min(c(initDBVect[bestInd], DB_lower, DB_upper))
	
	if(bestInd2 == 2){
		lowerExponent <- initExponents[bestInd] - 1
		upperExponent <- initExponents[bestInd]
	} else if(bestInd2 == 3){
		lowerExponent <- initExponents[bestInd]
		upperExponent <- initExponents[bestInd] + 1
	}
	
	# Step 3: 1D-optimization within reduced grid
	
	cat("Start 1D optimization")
	cat("\n")
	
	optim <- optimize(f = checkForceRatio, interval = c(10^lowerExponent, 10^upperExponent), x = x, classes = classes, law = law, steps = steps, springs = springs, multilevel = multilevel, print = print, tol = 10^lowerExponent)
	optimForceRatio <- optim$minimum
	
	return(optimForceRatio)
	
}

# Internal cost function for force ratio optimization
checkForceRatio <- function(forceRatio, x, classes, law = 0, steps = 10, springs = NULL, multilevel = TRUE, print = FALSE){
	
	springs <- do.optimFreeviz(x, classes, attractG = forceRatio, repelG = 1, law = law, steps = steps, springs = springs, multilevel = multilevel, print = print)
	rv <- do.radviz(cbind(x, classes), springs)
	DB_Idx <- DB_weightedIdx(rv)
	
	return(DB_Idx)
	
}
