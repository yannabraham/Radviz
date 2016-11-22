#' A smoothScatter function for Radviz objects
#' 
#' Plots the Dimensional Anchors and a smoothed color density representation
#' of projected data points in a 2D space.
#' 
#' @param x a radviz object as produced by do.radviz
#' @param main [Optional] a title to the graph, displayed on top
#' @param label.color The color of the Dimensional Anchors (defaults to orangered4)
#' @param label.size numeric character expansion factor for Dimensional Anchor labels
#'          multiplied by \code{par("cex")} yields the final character size.
#'          NULL and NA are equivalent to 1.0
#' @param mincnt numeric; cells with counts smaller than mincnt are not shown
#'          (see \link[hexbin]{grid.hexagons} for details)
#' @param color if color is not \code{NULL} and corresponds to one of the channels
#'          in the \code{hexcols} slot of the Radviz object, cells will be colored
#'          using colors in the \code{hexcols} slot
#' @param style character string specifying the type of plotting
#'          (see \link[hexbin]{grid.hexagons} for details)
#' 
#' @return Plots the result of a \code{\link{do.hex}} function
#' 
#' @examples
#' data(iris)
#' das <- c('Sepal.Length','Sepal.Width','Petal.Length','Petal.Width')
#' S <- make.S(das)
#' rv <- do.radviz(iris,S)
#' rv <- do.hex(rv,channels='Sepal.Length',ncols=4,use.quantile=TRUE)
#' hexplot(rv,color='Sepal.Length')
#' 
#' @author Yann Abraham
#' @author Dan Carr
#' @author Nicholas Lewin-Koh
#' @seealso \code{\link[hexbin]{grid.hexagons}} and \code{\link[hexbin]{hexbin}}
#'            for original implementation
#' @importFrom graphics par plot text title
#' @export
hexplot <- function(x,main=NULL,label.color='orangered4',label.size=1,mincnt=0,color=NULL,style='constant.col') {
	if(!'hex' %in% names(x)) {
		stop('Hexbin have not been run on radviz projection')
	}
	par(mar = c(0, 0, 1, 0))
	plot(x$springs, type = "n",xlab = "", 
			ylab = "", xlim = c(-1.1, 1.1), ylim = c(-1.1, 1.1), 
			frame.plot = F, axes = F)
	vps <- gridBase::baseViewports()
	grid::pushViewport(vps$inner)
	grid::pushViewport(vps$figure)
	grid::pushViewport(vps$plot)
	if(!is.null(color)) {
		if(color %in% names(x$hexcols)) {
			hexbin::grid.hexagons(x$hex,
					mincnt=mincnt,
					style=style,
					pen = x$hexcols[[color]][x$hex@count>=mincnt]
			)
		}
	} else {
		hexbin::grid.hexagons(x$hex,mincnt=mincnt)
	}
	text(x$springs, labels = dimnames(x$springs)[[1]],col=label.color,cex=label.size)
	if(!is.null(main)) {
		if(!is.null(color)) {
			title(main=paste(main,' (',color,')',sep=''	) )
		} else {
			title(main=main)
		}
	} else {
		title(main=color)
	}
	grid::popViewport()
	grid::popViewport()
	grid::popViewport()
}