#' Test if the object is a Radviz object
#' 
#' The function will return \code{TRUE} if the object is a Radviz object
#' 
#' @usage is.radviz(x)
#' 
#' @param x an object of class Radviz, as returned by \code{\link{do.radviz}}
#' 
#' @example examples/example-do.radviz.R
#' @examples
#' is.radviz(rv) # should be true
#' 
#' @author Yann Abraham
#' @export
is.radviz <- function(x) inherits(x,'radviz')
