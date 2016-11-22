#' A Plotting Function for the Radviz Object
#' 
#' Plots the Dimensional Anchors and projected data points in a 2D space.
#' 
#' @param x a radviz object as produced by \code{\link{do.radviz}}
#' @param main [Optional] a title to the graph, displayed on top
#' @param label.color The color of the Dimensional Anchors (defaults to orangered4)
#' @param label.size numeric character expansion factor for Dimensional Anchor labels;
#' multiplied by \code{par("cex")} yields the final character size. NULL and NA are equivalent to 1.0
#' @param point.color The point color (defaults to black)
#' @param point.shape The point shape (defaults to '.')
#' @param point.size The point size (defaults to 1)
#' @param add Logical: if add is \code{TRUE} then only the projected points are plotted
#' @param anchors.only plot only the anchors so that other plots can be freely overlaid
#' @param ...	further arguments to be passed to or from other methods
#' 
#' @details The \code{add} option allows plotting of additional data
#' such as cluster centers onto an existing plot.
#' 
#' @examples
#' data(iris) 
#' das <- c('Sepal.Length','Sepal.Width','Petal.Length','Petal.Width')
#' S <- make.S(das)
#' rv <- do.radviz(iris,S)
#' plot(rv,point.shape=1,point.color=c('red','green','blue')[as.integer(iris$Species)])
#' 
#' @author Yann Abraham
#' @importFrom graphics par plot text points
#' @export
plot.radviz <- function(x,main=NULL,
                        label.color='orangered4',label.size=1,
                        point.color='black',point.shape='.',point.size=1,
                        add=FALSE,anchors.only=FALSE,...) {
  par(mar = c(0,0,1,0))
  if (!add) {
    plot(x$springs, type = "n", main = main, xlab = "", 
         ylab = "", xlim = c(-1.1, 1.1), ylim = c(-1.1, 1.1), 
         frame.plot = F, axes = F)
    text(x$springs, labels = dimnames(x$springs)[[1]], 
         col = label.color,cex=label.size)
  }
  if(!anchors.only) {
    points(x$projected[x$valid,], pch = point.shape, col = point.color,
           cex = point.size)
  }
  if (add & anchors.only) {
    warning('add and anchors.only are both TRUE, so nothing will be plotted')
  }
}