#' @export
rv.da <-
function(springs,similarity) {
	proj <- do.radviz(similarity,springs)$projected
	proj <- proj[rownames(springs),]
	d <- rep(0,nrow(springs))
	for(i in seq(1,nrow(springs))) {
		d[i] <- sqrt(sum((proj[i,]-springs[i,])^2)) 
	}
	return(sum(d))
}
