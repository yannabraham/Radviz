subset.radviz <-
		function(x,i,...) {
	y$projected <- x$projected[i,]
	y$data <- x$data[i,]
	return(y)
}