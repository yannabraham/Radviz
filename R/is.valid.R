#' Identify the valid projections from a Radviz object
#' 
#' The function will return a vector as long as the data in x where points that could not be projected are \code{TRUE}
#' 
#' @param x an object of class Radviz, as returned by \code{\link{do.radviz}}
#' 
#' @example examples/example-is.valid.R
#' 
#' @author Yann Abraham
#' @export
is.valid <- function(x) return(!x$proj$data$rvalid)
