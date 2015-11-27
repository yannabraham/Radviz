#' Radviz Object Summary, head, Print, Subset Methods
#' 
#' Provides helper function to deal with Radviz objects
#' 
#' @usage is.radviz(x)
#' 
#' @param x an object of class Radviz, as returned by \code{\link{do.radviz}}
#' 
#' @examples
#' data(iris)
#' das <- c('Sepal.Length','Sepal.Width','Petal.Length','Petal.Width')
#' S <- make.S(das)
#' rv <- do.radviz(iris,S)
#' 
#' is.radviz(rv) # should be true
#' 
#' @author Yann Abraham
#' @export
is.radviz <- function(x) inherits(x,'radviz')
