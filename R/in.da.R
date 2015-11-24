in.da <-
function(springs,similarity) {
	cdist <- function(i,j,n) {
		ci <- min(i,j)
		cj <- max(i,j)
		return( min( abs(cj-ci), abs(n-cj+ci) ) )
	}
	similarity <- similarity[rownames(springs),rownames(springs)]
	neighbor <- matrix(0,nrow=nrow(springs),ncol=nrow(springs))
	for(i in seq(1,nrow(springs)-1)) {
		for(j in seq(i+1,nrow(springs))) {
			neighbor[i,j] <- neighbor[j,i] <- 1-2*cdist(i,j,nrow(springs))/nrow(springs)
		}
	}
	return(-sum(neighbor*similarity))
}
