#' Get the Result of the Optimization Operation
#' 
#' Once the order of anchors has been optimized using \code{\link{do.optimRadviz}}
#' this function can be used to recover the optimized anchors or any intermediate step
#' 
#' @param opt the result of the optimization operation performed by \code{\link{do.optimRadviz}}
#' @param n the optimized order of anchors to return; defaults to NULL, 
#' which returns the best identified combination
#' 
#' @return a character vector of the anchor names, ordered as in the n^th^ step of 
#' the optimization
#' 
#' @examples
#' data(iris)
#' das <- c('Sepal.Length','Sepal.Width','Petal.Length','Petal.Width')
#' S <- make.S(das)
#' sim.mat <- cosine(iris[,das])
#' in.da(S,sim.mat) # the starting value
#' new <- do.optimRadviz(S,sim.mat,iter=10,n=100)
#' get.optim(new) # the optimal order
#' get.optim(new,2) # the second step of the optimization
#' 
#' @author Yann Abraham
#' @export
get.optim <- function(opt,n=NULL) {
  N <- length(opt$perfs)
  if (is.null(n)) {
    n <- N
  }
  if (n>N) {
    stop(n,'the optimal anchor requested, only',N,'available\n')
  } else {
    return(opt$best[[n]])
  }
}