#' Compute the Cosine Similarity between the Columns of a Data Set
#' 
#' Given a dataset, compute the cosine similarity between to columns for use
#' in optimization of Dimensional Anchors
#' 
#' @param mat A matrix or data.frame
#' 
#' @details implementation by \href{https://stackoverflow.com/users/4288660/ekstroem}{ekstroem}
#'            (see \url{https://stackoverflow.com/a/45382926} for details)
#' 
#' @return A symmetrical matrix with as many rows as there are columns in input
#' 
#' @examples
#' data(iris)
#' das <- c('Sepal.Length','Sepal.Width','Petal.Length','Petal.Width')
#' mat <- iris[,das]
#' sim.mat <- cosine(mat)
#' ncol(mat)
#' dim(sim.mat)
#' 
#' @author Yann Abraham
#' @author David Ruau
#' 
#' @export
cosine <- function(mat) {
  if(!is.matrix(mat)) {
    mat <- as.matrix(mat)
  }
  res <- crossprod(mat)/(sqrt(tcrossprod(colSums(mat^2))))
  return(res)
}