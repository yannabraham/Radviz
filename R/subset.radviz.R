#' Subsetting a Radviz projection
#'
#' @param x a radviz object 
#' @param i A logical vector or expression evaluated on the Radviz object
#' @param ...	further arguments to be passed to or from other methods (not implemented)
#'
#' @return a new Radviz object containing only rows specified in i
#' 
#' @example examples/example-do.radviz.R
#' @examples
#' # subset rv
#' srv <- subset(rv,iris$Species=='setosa')
#' summary(srv)
#' sum(iris$Species=='setosa') # 50 objects in srv corresponding to setosa values
#' 
#' @author Yann Abraham
#' 
#' @importFrom ggplot2 `%+%`
#' @export
subset.radviz <- function(x,i=TRUE,...) {
  i <- eval(substitute(i),x$proj$data,parent.frame())
  x$proj <- x$proj %+% subset(x$proj$data,i)
  return(x)
}
