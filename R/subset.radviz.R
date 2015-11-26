#' Title
#'
#' @param x a radviz object 
#' @param i A logical or indices vector of the same length as the original data used to 
#'          create the Radviz object, that is used to subset each slots
#' @param ...	further arguments to be passed to or from other methods
#'
#' @return a new Radviz object containing only rows specified in i.
#'          Any density or hexbin analysis is dropped
#'
#' @examples
#' # create a radviz object
#' data(iris)
#' das <- c('Sepal.Length','Sepal.Width','Petal.Length','Petal.Width')
#' S <- make.S(das)
#' rv <- do.radviz(iris,S)
#' 
#' # subset rv
#' srv <- subset(rv,iris$Species=='setosa')
#' summary(srv)
#' sum(iris$Species=='setosa') # 50 objects in srv corresponding to setosa values
#' 
#' @author Yann Abraham
#' @export
subset.radviz <- function(x,...,i) {
		  y <- x['springs']
		  y$projected <- x$projected[i,]
		  y$data <- x$data[i,]
		  class(y) <- 'radviz'
	    return(y)
}