#' Radviz Summary
#' 
#' Provides a summary for Radviz objects
#' 
#' @param object an object of class Radviz, as returned by \code{\link{do.radviz}}
#' @param n the number of lines from each slots in the Radviz object to display (defaults to 6)
#' @param ...	further arguments to be passed to or from other methods
#' 
#' @examples
#' data(iris)
#' das <- c('Sepal.Length','Sepal.Width','Petal.Length','Petal.Width')
#' S <- make.S(das)
#' rv <- do.radviz(iris,S)
#' 
#' summary(rv)
#' 
#' @author Yann Abraham
#' @export
summary.radviz <- function(object,n=6,...) {
  cat('A Radviz object with',nrow(object$data),'objects and',nrow(object$springs),'dimensions\n')
  if(any(!object$valid)) {
    cat(sum(!object$valid),'point(s) could not be projected\n')
  }
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
