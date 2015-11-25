#' Computes 2D density for contour plots of Radviz objects
#' 
#' Computes 2D density estimate of projected data for a Radivz object,
#' using the \code{\link[MASS]{kde2d}} function from the \pkg{MASS} package
#' 
#' @param x a radviz object as produced by do.radviz
#' @param n Number of grid points in each direction. Can be scalar or a length-2
#' integer vector (see \link[MASS]{kde2d} for details)
#' 
#' @details Computes a 2D density estimate of Radviz projected data and stores the results
#' in a \code{density} slot of the Radviz object
#' 
#' @return the Radviz object with and extra slot \code{density} containing the 2D density
#' estimates for use with \code{\link{contour.radviz}}
#' 
#' @examples
#' data(iris)
#' das <- c('Sepal.Length','Sepal.Width','Petal.Length','Petal.Width')
#' S <- make.S(das)
#' rv <- do.radviz(iris,S)
#' rv <- do.density(rv)
#' contour(rv,point.shape=1,point.color=c('red','green','blue')[as.integer(iris$Species)])
#' 
#' @seealso \link{contour.radviz} for plotting
#' @author Yann Abraham
#' @export
do.density <-
function(x,n=50) {
	x$density <- MASS::kde2d(x$projected[,'x'],x$projected[,'y'],
			n=n,
			lims=c(c(-1.1,1.1),c(-1.1,1.1)))
	return(x)
}