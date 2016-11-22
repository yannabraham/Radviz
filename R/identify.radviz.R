#' Identify a Point in a Radviz Projection
#' 
#' Use this function to get the index of a point in a Radviz projection
#' 
#' @param x a radviz object as produced by \code{\link{do.radviz}}
#' @param n the number of points to identify, defaults to 1
#' @param ...	further arguments to be passed to or from other methods
#' 
#' @details The function will use the row names of the \code{data} variable in the \code{rv} object 
#' for labeling the plot
#' 
#' @return an integer vector containing the indices of the identified points, 
#' in the order they were identified.
#' 
#' @examples
#' data(iris) 
#' das <- c('Sepal.Length','Sepal.Width','Petal.Length','Petal.Width')
#' S <- make.S(das)
#' rv <- do.radviz(iris,S)
#' plot(rv,point.shape=1,point.color=c('red','green','blue')[as.integer(iris$Species)])
#' identify(rv)
#' 
#' @seealso \code{\link[graphics]{identify}}
#' 
#' @author Yann Abraham
#' @importFrom graphics identify
#' @export
identify.radviz <- function(x,...,n=1) {
  return(identify(x=x$projected$x,
                  y=x$projected$y,
                  labels=rownames(x$data),
                  n=n,
                  ...)
         )
}