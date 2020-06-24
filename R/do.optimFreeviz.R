#' Optimize the Dimensional Anchors Position using the Freeviz algorithm
#' 
#' Allows to compute the best arrangement of Dimensional Anchors so that
#' visualization efficiency (i.e. separation between classes) is maximized.
#' The Freeviz algorithm is implemented in C++ for optimal computational efficiency.
#' 
#' @param x Dataframe or matrix, with observations as rows and attributes as columns
#' @param classes Vector with class labels of the observations
#' @param attractG Number specifying the weight of the attractive forces
#' @param repelG  Number specifying the weight of the repulsive forces
#' @param law Integer, specifying how forces change with distance: 0 = (inverse) linear, 1 = (inverse) square
#' @param steps Number of iterations of the algorithm before re-considering convergence criterion
#' @param springs Numeric matrix with initial anchor coordinates. When \code{NULL} (=default), springs are initialized by \code{\link{make.S}}
#' @param multilevel Logical, indicating whether multi-level computation should be used. Setting it to TRUE can speed up computations
#' @param nClusters Number of clusters to be used at coarsest level of hierarchical tree (only used when \code{multilevel} is set to TRUE)
#' @param minTreeLevels Minimum number of clustering levels to consider (only used when \code{multilevel} is set to TRUE). This parameter might over-rule \code{nClusters} .
#' @param subsetting Logical, indicating whether a subsetting procedure should be used to compute the springs. The subset size is iteratively increased until the springs 
#' are found to be close enough to their true values, based on a confidence interval. For large datasets this option can considerably speed up computations.
#' @param minSamples Minimum number of samples to be considered for subsetting (only used when \code{subsetting} is set to TRUE)
#' @param print Logical, indicating whether information on the iterative procedure should be printed in the R console
#' 
#' @details Freeviz is an optimization method that finds the linear projection that
#' best separates instances of different classes, based on a physical metaphor. Observations are considered as physical particles,
#' that exert forces onto each other. Attractive forces occur between observations of the same class, and repulsive forces between
#' observations of different classes, with the force strength depending on the distance between observations. The goal of Freeviz
#' is to find the projection with minimal potential energy. For more details, see the original Freeviz paper: \url{http://dx.doi.org/10.1016/j.jbi.2007.03.010}
#' 
#' @return A matrix with 2 columns (x and y coordinates of dimensional anchors) and 1 line
#'          per dimensional anchor (so called springs).
#'          
#' @example examples/example-do.radviz.R
#' @examples
#' plot(rv,anchors.only=FALSE)
#' new.S <- do.optimFreeviz(x = iris[,das], classes = iris$Species)
#' new.rv <- do.radviz(iris,new.S)
#' plot(new.rv,anchors.only=FALSE)
#' 
#' @importFrom stats t.test
#' @importFrom Rcpp evalCpp
#' 
#' @author Nicolas Sauwen
#' @export
do.optimFreeviz <- function(x, classes, attractG = 1, repelG = 1, law = 0, steps = 10, springs = NULL, multilevel = FALSE, nClusters = 5000, minTreeLevels = 3, subsetting = FALSE, minSamples = 1000, print = TRUE){
		
	if(any(class(x) ==  "data.frame")) x <- as.matrix(x)
	if(!(law %in% c(0,1))) stop("Parameter 'law' not properly specified. Valid values are 0 or 1")
	
	# Sort data according to their classes
	classes_orig <- classes
	x_orig <- x
	classes <- as.integer(as.factor(classes))
	classesOrdered <- sort.int(classes, index.return = T)
	classes <- classesOrdered$x
	x <- x[classesOrdered$ix,]
	
	if(subsetting == TRUE){
		
		nDatapoints <- nrow(x)
		if(nDatapoints < minSamples) stop("Number of data points is smaller than specified minimum number of samples after subsetting")
		
		nSamples <- minSamples/2
		nRepeats <- 20
		springsMeanX <- c()
		springsMeanY <- c()
		converged <- FALSE
		
		while(!converged & nSamples < nDatapoints/2){
			
			# Increase subset size
			nSamples <- nSamples*2
			
			nClusters <- round(nSamples/10)
			reductionRatio <- nDatapoints/nSamples
			
			dataReduced <- stats::kmeans(x, centers = nClusters, iter.max = 30)
			clusterLabels <- dataReduced$cluster
			clusterCounts <- table(clusterLabels)
			
			sampleIndsMat <- matrix(1, nRepeats, nSamples)
			springsMatX <- matrix(0, ncol(x), nRepeats)
			springsMatY <- matrix(0, ncol(x), nRepeats)
			
			# Get clustered subsets and compute springs for each:
			for(i in 1:nRepeats){
				index <- 1
				for(j in 1:nClusters){
					nSamplesToAdd <- round(clusterCounts[j]/reductionRatio)
					if(j == nClusters){
						if(index + nSamplesToAdd > nSamples+1) nSamplesToAdd <- nSamples - index + 1
					}
					subsetVect <- which(clusterLabels == j)
					sampleIndsMat[i, index:(index+nSamplesToAdd-1)] <- sample(subsetVect, nSamplesToAdd)
					index <- index + nSamplesToAdd
				}
				dataSubset <- x[sampleIndsMat[i,], ]
				classesSubset <- classes[sampleIndsMat[i,]]
				springsTemp <- do.optimFreeviz(dataSubset, classesSubset, attractG, repelG, law, steps, multilevel = TRUE, nClusters = nClusters, minTreeLevels = minTreeLevels, print = print)
				springsMatX[,i] <- springsTemp[,1]
				springsMatY[,i] <- springsTemp[,2]
			}
			
			# Check if converged result is obtained for current subset size:
			springsMeanX <- apply(springsMatX, 1, mean)
			springsMeanY <- apply(springsMatY, 1, mean)
			springsDist <- sqrt((springsMatX - springsMeanX)^2 + (springsMatY - springsMeanY)^2)
			springsDistRel <- springsDist/(0.5*(sqrt(springsMatX^2 + springsMatY^2)+sqrt(springsMeanX^2 + springsMeanY^2)))
			springsDistDF <- as.data.frame(t(cbind(springsDist, -springsDist)))
			springsDistRelDF <- as.data.frame(t(cbind(springsDistRel, -springsDistRel)))
			
			converged <- TRUE
			
			for(i in 1:ncol(springsDistDF)){
				confIntervalAbs <- t.test(springsDistDF[,i])$"conf.int"
				confIntervalRel <- t.test(springsDistRelDF[,i])$"conf.int"
				if(confIntervalAbs[2] > 0.025 & confIntervalRel[2] > 0.1) converged <- FALSE
			}
			
		}
		
		print(paste0("Converged result at subset size = ", nSamples))
		
		freeVizSprings <- as.matrix(data.frame(springsMeanX, springsMeanY, row.names = colnames(x)))
		colnames(freeVizSprings) <- NULL
		
	} else if(multilevel == FALSE){
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

		if(iter == maxIters) warning("Maximum number of iterations reached without convergence")
		
	} else if(multilevel == TRUE){
		
		hclustList <- getHierarchicalClustList(x, classes)
		labels <- unique(classes)
		
		# Define parameters for the multi-level approach
		nDatapoints <- nrow(x)
		if(nClusters > nDatapoints/(2^(minTreeLevels-1))) nClusters <-  round(nDatapoints/(2^(minTreeLevels-1)))
		nClustersTemp <- nClusters
		treeLevelCount <- 1
		clusterLabelVect <- rep(0, nDatapoints)
		
		springs <-  make.S(colnames(x))
		classInds <- getClassIndices(classes)
		
#		For monitoring clustering quality:
#		DBIdxVect <- c()
#		DBIdxVect[1] <- DB_weightedIdx(x, springs, classes)
		
		while(nClustersTemp < nDatapoints*0.6 | treeLevelCount <= minTreeLevels-1){
			
			clusterRatio <- nDatapoints/nClustersTemp
			clusterLabelVect <- rep(0, nDatapoints)
			classesClustered <- c()
			clusterWeights <- c()
			clusterClassInd <- 1
			
			for(i in 1:length(hclustList)){
				nOneClassDataPoints <- length(hclustList[[i]]$order)
				nOneClassClusters <- round(nOneClassDataPoints/clusterRatio)
				treeCutOneClass <- cutree(hclustList[[i]], k = nOneClassClusters)
				clusterLabelVect[(classInds[i]+1):classInds[i+1]] <- treeCutOneClass + max(clusterLabelVect)
				classesClustered[clusterClassInd:(clusterClassInd+nOneClassClusters-1)] <- labels[i]
				clusterWeights[clusterClassInd:(clusterClassInd+nOneClassClusters-1)] <- table(treeCutOneClass)
				clusterClassInd <- clusterClassInd + nOneClassClusters
			}
			rownames(x) <- clusterLabelVect
			dataClustered <- rowsum(x, row.names(x), reorder = FALSE)
			dataClustered <- dataClustered/replicate(ncol(dataClustered), table(clusterLabelVect))		
			
			# Apply FreeViz on current level hierarchical tree:
			if(print) print(paste0("Hierarchical tree level ", treeLevelCount, ": ",  nrow(dataClustered), " clusters"))
			
			springs <- freeViz2(dataClustered, classesClustered, clusterWeights, attractG, repelG, law, steps, springs, print = print) 
			
#		DBIdxVect[treeLevelCount+1] <- DB_weightedIdx(data, springs, classes)
			
			# Updating step
			nClustersTemp <- nClustersTemp*2
			treeLevelCount <- treeLevelCount+1
		}
		
		# Final iteration on original dataset:
		if(print) print(paste0("Hierarchical tree level ", treeLevelCount, ": ",  nrow(x), " data points"))
		freeVizSprings <- do.optimFreeviz(x_orig, classes_orig, attractG, repelG, law, steps, springs, multilevel = FALSE, subsetting = FALSE, print = print) 		
	}
	
	return(freeVizSprings)
}



getClassIndices <- function(classes){
	
	classInds <- which(diff(classes) != 0)
	classInds <- c(0, classInds, length(classes))
	
	return(classInds)
}



# Obtain list with hierarchical clustering tree per class
# 
# @param data Dataframe or matrix, with observations as rows and attributes as columns
# @param classes Vector with class labels of the observations
# @return List with hierarchical clustering tree per class
# 
# @author Nicolas Sauwen
getHierarchicalClustList <- function(data, classes){
	
	dataNormalized <- apply(data,2,Radviz::do.L) 
	labels <- sort(unique(classes))
	hclustList <- vector("list", length(labels))
	
	for(i in 1:length(labels)){
		oneClassMat <- dataNormalized[which(classes == labels[i]),]
		oneClassDistMat <- dist(oneClassMat)
		hclustList[[i]] <- hclust(oneClassDistMat, method = "average")
	}
	
	return(hclustList)
}

# Internal Freeviz function for multilevel approach
freeViz2 <- function(x, classes, clustWeights, attractG = 1, repelG = 1, law = 0, steps = 10, springs = NULL, print = TRUE){
		
	if(any(class(x) ==  "data.frame")) x <- as.matrix(x)
	
	# Sort data according to their classes
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
		freeVizSprings <- optimizeAnchors2(dataNormalized, classInds, freeVizSprings, clustWeights, attractG, repelG, law, steps, normalizeExamples = 0)
		
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
	
	if(iter == maxIters) warning("Maximum number of iterations reached without convergence")
	
	return(freeVizSprings)
}



