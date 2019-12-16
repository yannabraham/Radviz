#' A smoothScatter function for Radviz objects
#' 
#' Plots the Dimensional Anchors and a smoothed color density representation
#' of projected data points in a 2D space.
#' 
#' @param x a radviz object as produced by do.radviz
#' @param main [Optional] a title to the graph, displayed on top
#' @param nbins the number of equally spaced bins for the binning computation (see \link[ggplot2]{geom_hex}
#'          for details)
#' @param color if color is not \code{NULL} and corresponds to one of the channels
#'          in the \code{hexcols} slot of the Radviz object, cells will be colored
#'          using colors in the \code{hexcols} slot
#' 
#' @return the internal ggplot2 object plus added layers, allowing for extra geoms to be added
#' 
#' @example examples/example-do.radviz.R
#' @examples
#' hexplot(rv,color='Sepal.Length')
#' 
#' @author Yann Abraham
#' @importFrom ggplot2 ggtitle aes_string scale_fill_gradient geom_hex stat_summary_hex
#' @export
hexplot <- function(x,
                    main=NULL,
                    nbins=30,
                    color=NULL) {
  p <- x$proj+
    ggtitle(main)+
    scale_fill_gradient(low='grey90',high='dodgerblue4')
  
  if(is.null(color)) {
    slayer <- geom_hex(bins=nbins)
  } else {
    slayer <- stat_summary_hex(aes_string(z=color),
                               fun=median,
                               bins=nbins)
  }
  p$layers <- c(slayer,p$layers)
  return(p)
}