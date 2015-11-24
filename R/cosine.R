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
