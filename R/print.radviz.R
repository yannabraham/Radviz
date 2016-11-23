#' Print Radviz Object
#' 
#' Prints the first few lines of the data used to generate a Radviz projection
#' 
#' @param x an object of class Radviz, as returned by \code{\link{do.radviz}}
#' @param n the number of lines from each slots in the Radviz object to display (defaults to 6)
#' @param ...	further arguments to be passed to or from other methods
#' 
#' 
#' @examples
#' data(iris)
#' das <- c('Sepal.Length','Sepal.Width','Petal.Length','Petal.Width')
#' S <- make.S(das)
#' rv <- do.radviz(iris,S)
#' 
#' print(rv)
#' 
#' @author Yann Abraham
#' @importFrom utils head
#' @export
print.radviz <- function(x,n=6,...) {
  cat('A radviz object with',nrow(x$springs),'dimensions and',nrow(x$data),'objects generated using\n')
  print(head(x$data,n))
}
