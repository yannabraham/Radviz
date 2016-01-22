#' Get the Result of the Optimization Operation
#' 
#' Once the order of anchors has been optimized using \code{\link{do.optim}}
#' this function can be used to recover the optimized anchors or any intermediate step
#' 
#' @param opt the result of the optimization operation performed by \code{\link{do.optim}}
#' @param n the optimized order of anchors to return; defaults to NULL, 
#' which returns the best identified combination
#' 
#' @return a character vector of the anchor names, ordered as in the n^th^ step of 
#' the optimization
#' 
#' @examples
#' #' data(iris)
#' das <- c('Sepal.Length','Sepal.Width','Petal.Length','Petal.Width')
#' S <- make.S(das)
#' scaled <- apply(iris[,das],2,do.L)
#' rv <- do.radviz(scaled,S)
#' plot(rv,main='Iris Columns',
#'      point.shape=1,
#'      point.color=c('red','green','blue')[as.integer(iris$Species)])
#' sim.mat <- cosine(scaled)
#' in.da(S,sim.mat) # the starting value
#' new <- do.optim(S,sim.mat,iter=10,n=100)
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
    stop(n,'th optimal anchor requested, only',N,'available\n')
  } else {
    return(opt$best[[n]])
  }
}