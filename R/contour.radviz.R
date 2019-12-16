#' 
#' Plots the Dimensional Anchors and density lines for projected data points in a 2D space.
#' 
#' @param x a radviz object as produced by do.radviz
#' @param main [Optional] a title to the graph, displayed on top 
#' @param color the variable in the Radviz projection used to color the contours
#' @param size The thickness of contour lines
#' @param ...	further arguments to be passed to or from other methods (not implemented)
#' 
#' @return the internal ggplot2 object plus added layers, allowing for extra geoms to be added
#'  
#' @example examples/example-do.radviz.R
#' @examples 
#' contour(rv,color='Species')
#' 
#' @author Yann Abraham
#' @keywords multivariate hplot
#' @importFrom ggplot2 ggtitle geom_density2d aes_string
#' @export
contour.radviz <- function(x,...,
                           main=NULL,
                           color=NULL,
                           size=0.5) {
  p <- x$proj+
    ggtitle(main)
  
  slayer <- geom_density2d(aes_string(color=color),
                           size=size)
  p$layers <- c(slayer,p$layers)
  return(p)
}
