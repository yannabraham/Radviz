#' Subsetting a Radviz projection
#'
#' @param x a radviz object 
#' @param i A logical or indices vector of the same length as the original data used to 
#'          create the Radviz object, that is used to subset each slots
#' @param ...	further arguments to be passed to or from other methods (not implemented)
#'
#' @return a new Radviz object containing only rows specified in i.
#'          Any density or hexbin analysis is dropped
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
subset.radviz <- function(x,i,...) {
		  x$proj <- x$proj %+% subset(x$proj$data,i)
	    return(x)
}