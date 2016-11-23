#' Dimensions of a Radviz Object
#' 
#' Retrieves the dimensions of a Radviz projection
#' 
#' @param x an object of class Radviz, as returned by \code{\link{do.radviz}}
#' 
#' @examples
#' data(iris)
#' das <- c('Sepal.Length','Sepal.Width','Petal.Length','Petal.Width')
#' S <- make.S(das)
#' rv <- do.radviz(iris,S)
#' 
#' dim(rv)
#' nrow(rv) # for free!
#' 
#' @author Yann Abraham
#' @export
dim.radviz <- function(x) {
  dim(x$data)
}