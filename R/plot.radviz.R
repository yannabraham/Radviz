#' A Plotting Function for the Radviz Object
#' 
#' Plots the Dimensional Anchors and projected data points in a 2D space.
#' 
#' @param x a radviz object as produced by \code{\link{do.radviz}}
#' @param main [Optional] a title to the graph, displayed on top
#' @param anchors.only by default only plot the anchors so that other methods can easily be chained
#' @param ...	further arguments to be passed to or from other methods (not implemented)
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
                        ...) {
  if(!is.null(main)) {
    p <- x$proj+
      ggtitle(main)
  } 
  if(anchors.only) { 
    p <- x$proj
  }
  else {
    p <- x$proj+
      geom_point()
  }
  return(p)
}