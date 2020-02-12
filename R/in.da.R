#' Optimization functions for Dimensional Anchors in Radviz
#' 
#' Visual efficiency of Radviz plots depends heavily on the correct arrangement of Dimensional Anchors.
#' These functions implement the optimization strategies described in
#' \href{http://link.springer.com/chapter/10.1007/978-3-642-13672-6_13}{Di Caro et al 2012}
#' 
#' @usage in.da(springs, similarity)
#' rv.da(springs, similarity)
#' 
#' @param springs A matrix of 2D dimensional anchor coordinates, as returned by \code{\link{make.S}}
#' @param similarity A similarity matrix measuring the correlation between Dimensional Anchors
#' 
#' @return A measure of the efficiency of the Radviz projection of the similarity matrix
#'          onto a set of springs
#' 
#' @details Following the recommendation of Di Caro *et al.* we used a cosine function to calculate
#'            the similarity between Dimensional Anchors (see \code{\link{cosine}} for details).
#'            The in.da function implements the independent similarity measure,
#'            where the value increases as the Radviz projection improves.
#'            The rv.da function implements the radviz-dependent similarity measure,
#'            where the value decreases as the Radviz projection improves. 
#' 
#' @examples 
#' data(iris)
#' das <- c('Sepal.Length','Sepal.Width','Petal.Length','Petal.Width')
#' S <- make.S(das)
#' mat <- iris[,das]
#' sim.mat <- cosine(mat)
#' in.da(S,sim.mat)
#' rv.da(S,sim.mat)
#' 
#' @aliases in.da rv.da
#' @author Yann Abraham
#' @export
in.da <- function(springs,similarity) {
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

#' @export
rv.da <- function(springs,similarity) {
  proj <- do.radviz(similarity,springs)$proj$data[,c('rx','ry')]
  proj <- proj[rownames(springs),]
  d <- rep(0,nrow(springs))
  for(i in seq(1,nrow(springs))) {
    d[i] <- sqrt(sum((proj[i,]-springs[i,])^2)) 
  }
  return(sum(d))
}
