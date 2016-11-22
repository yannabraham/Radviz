#' Creates a contour plot, or add a contour plot to an existing plot for a Radviz Object
#' 
#' Plots the Dimensional Anchors and density lines for projected data points in a 2D space.
#' 
#' @param x a radviz object as produced by do.radviz
#' @param main [Optional] a title to the graph, displayed on top 
#' @param label.color The color of the Dimensional Anchors (defaults to orangered4)
#' @param label.size numeric character expansion factor for Dimensional Anchor labels;
#'          multiplied by \code{par("cex")} yields the final character size.
#'          NULL and NA are equivalent to 1.0
#' @param contour.color The color of contour lines (defaults to \code{par('fg')})
#' @param contour.size The thickness of contour lines (defaults to \code{par('lwd')})
#' @param point.color The point color (defaults to black)
#' @param point.shape The point shape (defaults to '.')
#' @param point.size the point size (defaults to 1)
#' @param add Logical: if add is \code{TRUE} then only the contour lines are plotted
#' @param drawlabels Logical. Contours are labelled if \code{TRUE}
#' @param drawpoints Logical: if \code{TRUE} then the projected points are plotted
#' @param ...	further arguments to be passed to or from other methods
#' 
#' @details
#'  The density lines will be calculated before plotting, if the Radviz object does not have
#'  one yet. The add allows plotting of contour lines over existing data, 
#'  either the one used to generate the density or a different one (for context).
#' 
#' 
#' @return Invisibly, the Radviz object that has been used as input; useful when
#'          \code{\link{do.density}} has not been called before so that results can be recovered
#'  
#' @examples 
#' data(iris)
#' das <- c('Sepal.Length','Sepal.Width','Petal.Length','Petal.Width')
#' S <- make.S(das)
#' rv <- do.radviz(iris,S)
#' rv <- do.density(rv)
#' contour(rv,point.shape=1,point.color=c('red','green','blue')[as.integer(iris$Species)])
#' 
#' @seealso \link{do.density} for details about mapping projection to density
#' 
#' @author Yann Abraham
#' @keywords multivariate hplot
#' @importFrom graphics par plot text points contour
#' @export
contour.radviz <- function(x,...,main=NULL,label.color='orangered4',label.size=1,
                           contour.color=par("fg"),contour.size=par('lwd'),
                           point.color='lightgrey',point.shape='.',point.size=1,
                           add=F,drawlabels=FALSE,drawpoints=FALSE) {
	par(mar = c(0,0,1,0))
	if (!add) {
		plot(x$springs, type = "n", main = main, xlab = "", 
				ylab = "", xlim = c(-1.1, 1.1), ylim = c(-1.1, 1.1), 
				frame.plot = F, axes = F)
		text(x$springs, labels = dimnames(x$springs)[[1]], 
				col = label.color,cex=label.size)
	}
	if(!'density' %in% names(x)) {
		x <- do.density(x) 
	}
	if(drawpoints) {
		points(x$projected, pch = point.shape, col = point.color, 
				cex = point.size)
	}
	contour(x$density,drawlabels=drawlabels,add=T,axes=F,frame.plot=F,col=contour.color,lwd=contour.size)
	return(invisible(x))
}
