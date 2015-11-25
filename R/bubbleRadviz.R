#' A Plotting Function for the Radviz Object
#' 
#' Plots the Dimensional Anchors and projected data points in a 2D space.
#' 
#' @param x a radviz object as produced by do.radviz
#' @param main *Optional* a title to the graph, displayed on top
#' @param label.color The color of the Dimensional Anchors (defaults to orangered4)
#' @param label.size numeric character expansion factor for Dimensional Anchor labels;
#'          multiplied by \code{par("cex")} yields the final character size. NULL and NA are equivalent to 1.0
#' @param bubble.color The color of the bubble, either a single color or a vector of colors (defaults to grey)
#' @param bubble.fg The foreground color of the bubble, either a single color or a vector of colors (defaults to white)
#' @param bubble.size the bubble size, either a single number of a vector of values (defaults to 1)
#' @param scale A scaling factor that will be applied to bubble.size (see \code{\link{symbols}}
#'          for details)
#' @param decreasing How the bubbles should be sorted: either \code{NA} for no sorting,
#'          \code{TRUE} or \code{FALSE} for sorting by decreasing or increasing 
#'          bubble.size respecitvely, or a vector specifying the bubble order
#'          (see \code{\link{symbols}} for details)
#' @param add Logical: if add is \code{TRUE} than only the projected points are plotted
#' 
#' @details This function allows for the projection of clusters in Radviz (for example results of
#'            the SPADE algorithm), where the cluster size is derived from the number of events
#'            that fall into a specific cluster
#' 
#' @examples
#' data(iris)
#' das <- c('Sepal.Length','Sepal.Width','Petal.Length','Petal.Width')
#' S <- make.S(das)
#' species <- apply(iris[,c('Sepal.Length','Sepal.Width','Petal.Length','Petal.Width')],
#'                  2,
#'                  tapply,iris$Species,median)
#' rv <- do.radviz(species,S)
#' bubbleRadviz(rv,
#'              bubble.color=c('red','green','blue')[seq(1,length(levels(iris$Species)))],
#'              bubble.size=table(iris$Species),
#'              decreasing=TRUE)
#' 
#' @author Yann Abraham
#' @keywords multivariate cluster hplot
#' @export
bubbleRadviz <-
function(x, main = NULL, label.color = "orangered4", label.size=1, bubble.color = "grey",
         bubble.fg='white', bubble.size = 1, scale=0.5, decreasing=NA, add=FALSE) {
	if(length(bubble.color)==1) {
		bubble.color <- rep(bubble.color,nrow(x$projected))
	}
	if(length(bubble.size)==1) {
		bubble.size <- rep(bubble.size,nrow(x$projected))
	}
	par(mar = c(0,0,1,0))
	if(!add) {
		plot(x$springs, type = "n",xlab = "", 
				ylab = "", xlim = c(-1.1, 1.1), ylim = c(-1.1, 1.1), 
				frame.plot = F, axes = F, main = main
		)
		text(x$springs,
				labels = dimnames(x$springs)[[1]],
				col=label.color,
				cex=label.size
		)
	}
	if(!is.na(decreasing)) {
		if(is.logical(decreasing)) {
			x$projected <- x$projected[order(bubble.size,decreasing=decreasing),]
			bubble.color <- bubble.color[order(bubble.size,decreasing=decreasing)]
			bubble.size <- sort(bubble.size,decreasing=decreasing)
		} else {
			if(length(decreasing)==nrow(x$projected)) {
				x$projected <- x$projected[decreasing,]
				bubble.color <- bubble.color[decreasing]
				bubble.size <- bubble.size[decreasing]
			}
		}
	}
	symbols(x$projected,circles=bubble.size,bg=bubble.color,fg=bubble.fg,inches=scale,add=!add)
	return(invisible(NULL))
}