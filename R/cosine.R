#' Compute the Cosine Similarity between the Columns of a Data Set
#' 
#' Given a dataset, compute the cosine similarity between to columns for use
#' in optimization of Dimensional Anchors
#' 
#' @param mat A matrix or data.frame
#' 
#' @return A symmetrical matrix with as many rows as there are columns in input
#' 
#' @examples
#' data(iris)
#' das <- c('Sepal.Length','Sepal.Width','Petal.Length','Petal.Width')
#' mat <- iris[,das]
#' scaled <- apply(mat,2,do.L)
#' sim.mat <- cosine(scaled)
#' ncol(mat)
#' dim(sim.mat)
#' 
#' @author Yann Abraham
#' @export
cosine <-
function(mat) {
	do.cos <- function(A,B) {
		return(
				A%*%B/(sqrt(sum(A^2))*sqrt(sum(B^2)))
		)
	}
	res <- matrix(0,nrow=ncol(mat),ncol=ncol(mat))
	for(i in seq(1,ncol(mat)-1)) {
		for(j in seq(i+1,ncol(mat))) {
			if(i!=j) {
				
				res[i,j] <- res[j,i] <- do.cos(mat[,i],mat[,j])
			}
		}
	}
	diag(res) <- 1
	# res[lower.tri(res)] <- res[upper.tri(res)]
	dimnames(res) <- list(dimnames(mat)[[2]],dimnames(mat)[[2]])
	return(res)
}
