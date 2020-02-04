
#' Projects a Matrix or a Data Frame to a 2D space using Freeviz algorithm
#' 
#' @param x Dataframe or matrix, with observations as rows and attributes as columns
#' @param classes Vector with class labels of the observations
#' @param attractG Number specifying the weight of the attractive forces
#' @param repelG  Number specifying the weight of the repulsive forces
#' @param law Integer, specifying how forces change with distance: 0 = (inverse) linear, 1 = (inverse) square
#' @param steps Number of iterations of the algorithm before re-considering convergence criterion
#' @param springs Numeric matrix with initial anchor coordinates. When \code{NULL} (=default), springs are initialized by \code{\link{make.S}}
#' @param print Logical, indicating whether information on the iterative procedure should be printed in the R console
#' 
#' @return An object of class radviz with the following slots:
#'          \itemize{
#'            \item \code{data} the original data (\code{x})
#'            \item \code{springs} the computed \code{springs}
#'            \item \code{projected} the projection of \code{x} on \code{springs},
#'                    a matrix of 2D coordinates for every line in df
#'            \item \code{valid} a logical vector 
#'  		  \item \code{type} character string, indicating method used for computing the projection (in this case "freeviz")
#'            \item \code{classes} vector with class labels of the observations
#'          }
#' @examples
#' # the first example generates a simple Radviz object
#' data(iris)
#' fv <- do.freeviz(x = iris[,1:4], classes = iris$Species)
#' summary(fv)
#' 
#' @author Nicolas Sauwen
#' @export
do.freeviz <- function(x, classes, attractG = 1, repelG = 1, law = 0, steps = 10, springs = NULL, print = TRUE){
	
#	tStart <- Sys.time()
	
	if(class(x) ==  "data.frame") x <- as.matrix(x)
	
	# Sort data according to their classes
	classes_orig <- classes
	x_orig <- x
	classes <- as.integer(as.factor(classes))
	classesOrdered <- sort.int(classes, index.return = T)
	classes <- classesOrdered$x
	x <- x[classesOrdered$ix,]
	
	if(is.null(springs)) springs <-  make.S(colnames(x))
#	springs <- springs[c(2,1,4,3),] # Temporary, to compare results with Python
	
	# Get indices where class changes
	classInds <- getClassIndices(classes)
	
	dataNormalized <- apply(x,2, do.L) 
	dataNormalized <- t(dataNormalized) # C++ code currently assumes observations as columns and attributes as rows
	freeVizSprings <- springs
	
	maxIters <- 1e5
	iter <- 1
	converged <- FALSE
	convTol <- 1e-3
	
	while(!converged & iter < maxIters){
		
		oldSprings <- freeVizSprings
		freeVizSprings <- optimizeAnchors(dataNormalized, classInds, freeVizSprings, attractG, repelG, law, steps, normalizeExamples = 0)
		
		# Avoid axes being suppressed in the first convergence steps:
		if(iter < 5){
			axesLengths <- sqrt(apply(freeVizSprings^2, 1, sum))
			suppressedInds <- which(axesLengths < 1e-2)
			if(length(suppressedInds) > 0){
				freeVizSprings <-  pracma::flipdim(springs, 1)
			}
		}
		
		springsDiff <- sqrt(apply((oldSprings - freeVizSprings)^2, 1, sum))
		if(max(springsDiff) < convTol) converged <- TRUE
		iter <- iter + 1
	}
	
	rownames(freeVizSprings) <- rownames(springs)
#	colnames(freeVizSprings) <- c("x","y")
	
	if(print) print(paste0("# iters: ", iter))
#	timeDiff <- Sys.time() - tStart
#	print(paste0("Computation time: ", timeDiff)) # Temporary, for comparison with Python code
	if(iter == maxIters) warning("Maximum number of iterations reached without convergence")
	
	# Create Radviz object
	radvizObject <- do.radviz(as.data.frame(x_orig), freeVizSprings, type = "freeviz", classes = classes_orig)
	
	return(radvizObject)
}



getClassIndices <- function(classes){
	
	classInds <- which(diff(classes) != 0)
	classInds <- c(0, classInds, length(classes))
	
	return(classInds)
}