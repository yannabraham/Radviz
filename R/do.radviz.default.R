do.radviz.default <-
function(x,springs,...) {
	radviz <- list()
	radviz$data <- x
	radviz$springs <- springs
	mat <- as.matrix(x[,rownames(springs)])
	weights <- mat/matrix(rep(rowSums(mat),each=ncol(mat)),nrow=nrow(mat),byrow=T)
	rx <- colSums(t(weights)*springs[,1])
	ry <- colSums(t(weights)*springs[,2])
	proj <- data.frame(x=rx,y=ry)
	row.names(proj) <- row.names(mat)
	radviz$projected <- proj
	class(radviz) <- 'radviz'
	return(radviz)
}
