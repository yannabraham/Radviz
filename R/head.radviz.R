#' Radviz Object Summary, head, Print, Subset Methods
#' 
#' Provides helper function to deal with Radviz objects
#' 
#' @usage head.radviz(x,...,n=6)
#' is.radviz(x)
#' print.radviz(x,...,n=6)
#' summary.radviz(object,...,n=6)
#' 
#' @param x an object of class Radviz, as returned by \code{\link{do.radviz}}
#' @param object an object of class Radviz, as returned by \code{\link{do.radviz}}
#' @param n the number of lines from each slots in the Radviz object to display (defaults to 6)
#' @param ...	further arguments to be passed to or from other methods
#' 
#' @details \code{head.radviz} shows the first \code{n} lines of the radviz \code{data} object.
#' 
#' @examples
#' data(iris)
#' das <- c('Sepal.Length','Sepal.Width','Petal.Length','Petal.Width')
#' S <- make.S(das)
#' rv <- do.radviz(iris,S)
#' 
#' is.radviz(rv) # should be true
#' summary(rv)
#' head(rv)
#' print(rv)
#' 
#' @aliases is.radviz print.radviz summary.radviz
#' @importFrom utils head
#' @author Yann Abraham
#' @export
head.radviz <- function(x,...,n=6) {
	print(head(x$data,n=n))
}

#' @export
is.radviz <- function(x) inherits(x,'radviz')

#' @importFrom utils head
#' @export
print.radviz <- function(x,...,n=6) {
  cat('A radviz object with',length(x$springs),'dimensions generated using\n')
  print(head(x$data,n))
}

#' @export
summary.radviz <- function(object,...,n=6) {
  cat('A Radviz object with',nrow(object$data),'objects and',ncol(object$data),'dimensions\n')
  das <- rownames(object$springs)
  if(length(das)>n) {
    das <- das[seq(1,n)]
    das <- c(das,'...')
  }
  cat('Dimensional Anchors are',das,'\n')
  if('hex' %in% names(object)) {
    if('hexcols' %in% names(object)) {
      cat('an hexbin analysis is available with the following colors:\n')
      cat('\t',names(object$hexcols),'\n')
    } else {
      cat('an hexbin analysis is available\n')
    }
  }
  if('density' %in% names(object)) {
    cat('a density analysis is available\n')
  }
}
