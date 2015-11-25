#' Radviz Object Summary, Head, Print, Subset Methods
#' 
#' Provides helper function to deal with Radviz objects
#' 
#' @usage summary(x,n=8)
#' head(x,n=6)
#' print(x,n=6)
#' subset(x,i)
#' 
#' @param x An object of class Radviz, as returned by \code{link{do.radviz}}
#' @param n The number of lines from each slots in the Radviz object to display (defaults to 6)
#' @param i A logical or indices vector of the same length as the original data used to 
#'          create the Radviz object, that is used to subset each slots
#' 
#' @return For subset.radviz, a new Radviz object containing only rows specified in i.
#'          Any density or hexbin analysis is dropped
#' 
#' @examples
#' data(iris)
#' das <- c('Sepal.Length','Sepal.Width','Petal.Length','Petal.Width')
#' S <- make.S(das)
#' rv <- do.radviz(iris,S)
#' summary(rv)
#' head(rv,5)
#' print(rv)
#' # create a subset
#' srv <- subset(rv,iris$Species=='setosa')
#' summary(srv)
#' sum(iris$Species=='setosa') # 50 objects in srv corresponding to setosa values
#' 
#' @aliases summary.radviz head.radviz print.radviz subset.radviz
#' @author Yann Abraham
#' @export
summary.radviz <-
function(x,n=8) {
	cat('A Radviz object with',nrow(x$data),'objects and',ncol(x$data),'dimensions\n')
	das <- rownames(x$springs)
	if(length(das)>n) {
		das <- das[seq(1,n)]
		das <- c(das,'...')
	}
	cat('Dimensional Anchors are',das,'\n')
}
