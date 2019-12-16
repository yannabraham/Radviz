#' A smoothScatter function for Radviz objects
#' 
#' Plots the Dimensional Anchors and a smoothed color density
#' representation of projected data points in a 2D space.
#' 
#' @param x a radviz object as produced by \code{\link{do.radviz}}
#' @param main [Optional] a title to the graph, displayed on top
#' @param smooth.color the gradient will be generated from \code{white} to \code{smooth.color}
#' @param nbin the number of equally spaced grid points for the density estimation (see \link[ggplot2]{geom_density_2d}
#'          for details)
#' @param ...	further arguments to be passed to or from other methods (not implemented)
#' 
#' @return the internal ggplot2 object plus added layers, allowing for extra geoms to be added
#' 
#' @example examples/example-do.radviz.R
#' @examples
#' smoothRadviz(rv)
#' 
#' @author Yann Abraham
#' @importFrom ggplot2 ggtitle stat_density2d aes_string scale_fill_continuous guides
#' @export
smoothRadviz <- function (x, 
                          main = NULL,
                          smooth.color = "dodgerblue4",
                          nbin=200) {
  p <- x$proj+
    ggtitle(main)+
    scale_fill_continuous(low = "white", high = smooth.color)+
    guides(fill=FALSE)
  
  slayer <- stat_density2d(aes_string(fill = "..density..^0.25"),
                           geom = "tile",
                           contour = FALSE,
                           n = nbin)
  
  p$layers <- c(slayer,p$layers)
  
  return(p)
}