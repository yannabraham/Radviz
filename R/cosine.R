#' Compute the Cosine Similarity between the Columns of a Data Set
#' 
#' Given a dataset, compute the cosine similarity between to columns for use
#' in optimization of Dimensional Anchors
#' 
#' @param mat A matrix or data.frame
#' 
#' @details implementation by David Ruau (see \url{https://gist.github.com/bobthecat/2903031} for details)
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
#' @author David Ruau
#' 
#' @export
cosine <- function(mat) {
  dotmat <- t(mat) %*% mat
  res <- dotmat / (sqrt(diag(dotmat)) %*% t(sqrt(diag(dotmat))))
  return(res)
}