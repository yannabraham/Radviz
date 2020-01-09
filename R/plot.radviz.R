#' A Plotting Function for the Radviz Object
#' 
#' Plots the Dimensional Anchors and projected data points in a 2D space.
#' 
#' @param x a radviz object as produced by \code{\link{do.radviz}}
#' @param main [Optional] a title to the graph, displayed on top
#' @param anchors.only by default only plot the anchors so that other methods can easily be chained
#' @param ...	further arguments to be passed to or from other methods (not implemented)
#' @param label.color deprecated, see \code{\link{do.radviz}}
#' @param label.size deprecated, see \code{\link{do.radviz}}
#' @param point.color deprecated, use \code{\link{geom_point}} instead
#' @param point.shape deprecated, use \code{\link{geom_point}} instead
#' @param point.size deprecated, use \code{\link{geom_point}} instead
#' @param add deprecated, use \code{\link{geom_point}} instead
#' 
#' @details by default the plot function only shows the anchors. Extra geoms are required to display the data
#' 
#' @return the internal ggplot2 object, allowing for extra geoms to be added
#' 
#' @example examples/example-do.radviz.R
#' @examples
#' plot(rv)
#' plot(rv,anchors.only=FALSE)
#' 
#' library(ggplot2)
#' ## should look the same as before
#' plot(rv)+geom_point()
#' plot(rv)+geom_point(aes(color=Species))
#' 
#' @author Yann Abraham
#' @importFrom ggplot2 ggtitle geom_point
#' @export
plot.radviz <- function(x,
                        main=NULL,
                        anchors.only=TRUE,
                        label.color,
                        label.size,
                        point.color,
                        point.shape,
                        point.size,
                        add,
                        ...) {
  ## check for deprecated arguments
  if(!missing(label.color))
    warning('label.color is a deprecated argument, use plot(x)+geom_point() and custom aes() to change plot.',call. = FALSE)
  if(!missing(label.size))
    warning('label.size is a deprecated argument, use plot(x)+geom_point() and custom aes() to change plot.',call. = FALSE)
  if(!missing(point.color))
    warning('point.color is a deprecated argument, use plot(x)+geom_point() and custom aes() to change plot.',call. = FALSE)
  if(!missing(point.shape))
    warning('point.shape is a deprecated argument, use plot(x)+geom_point() and custom aes() to change plot.',call. = FALSE)
  if(!missing(point.size))
    warning('point.size is a deprecated argument, use plot(x)+geom_point() and custom aes() to change plot.',call. = FALSE)
  if(!missing(add))
    warning('add is a deprecated argument, use plot(x)+geom_point() and custom aes() to change plot.',call. = FALSE)
  ## plot
  p <- x$proj
  if(!is.null(main)) {
    p <- p + ggtitle(main)
  } 
  if(!anchors.only) { 
    p <- p + geom_point()
  }
  return(p)
}
