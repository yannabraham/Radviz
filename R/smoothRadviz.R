#' A smoothScatter function for Radviz objects
#' 
#' Plots the Dimensional Anchors and a smoothed color density
#' representation of projected data points in a 2D space.
#' 
#' @param x a radviz object as produced by \code{\link{do.radviz}}
#' @param main [Optional] a title to the graph, displayed on top
#' @param color the gradient will be generated from \code{white} to \code{color}
#' @param nbin the number of equally spaced grid points for the density estimation (see \link[ggplot2]{geom_density_2d}
#'          for details)
#' @param label.color the color of springs for visualization
#' @param label.size the size of the anchors (see \href{https://ggplot2.tidyverse.org/articles/articles/faq-customising.html}{customizing ggplot2} for details on default value)
#' @param smooth.color deprecated, see \code{\link{stat_density2d}} instead
#' @param max.dens deprecated, see \code{\link{stat_density2d}} instead
#' @param transformation deprecated, see \code{\link{stat_density2d}} instead
#' @param nrpoints deprecated, see \code{\link{stat_density2d}} instead
#' @param ncols deprecated, see \code{\link{stat_density2d}} instead
#' @param bandwidth deprecated, see \code{\link{stat_density2d}} instead
#' 
#' @return the internal ggplot2 object plus added layers, allowing for extra geoms to be added
#' 
#' @example examples/example-do.radviz.R
#' @examples
#' smoothRadviz(rv)
#' 
#' @author Yann Abraham
#' @importFrom ggplot2 stat_density2d aes_string scale_fill_continuous guides 
#' @export
smoothRadviz <- function (x, 
                          main = NULL,
                          color = "dodgerblue4",
                          nbin=200,
                          label.color=NULL,
                          label.size=NULL,
                          smooth.color,
                          max.dens,
                          transformation,
                          nrpoints,
                          ncols,
                          bandwidth) {
  ## check for deprecated arguments
  if(!missing(smooth.color))
    warning("smooth.color is a deprecated argument, use plot(x)+stat_density2d(geom='tile') and custom aes() to change plot.",call. = FALSE)
  if(!missing(max.dens))
    warning("max.dens is a deprecated argument, use plot(x)+stat_density2d(geom='tile') and custom aes() to change plot.",call. = FALSE)
  if(!missing(transformation))
    warning("transformation is a deprecated argument, use plot(x)+stat_density2d(geom='tile') and custom aes() to change plot.",call. = FALSE)
  if(!missing(nrpoints))
    warning("nrpoints is a deprecated argument, use plot(x)+stat_density2d(geom='tile') and custom aes() to change plot.",call. = FALSE)
  if(!missing(ncols))
    warning("ncols is a deprecated argument, use plot(x)+stat_density2d(geom='tile') and custom aes() to change plot.",call. = FALSE)
  if(!missing(bandwidth))
    warning("bandwidth is a deprecated argument, use plot(x)+stat_density2d(geom='tile') and custom aes() to change plot.",call. = FALSE)
  ## plot
  p <- plot.radviz(x,
                   main = main,
                   label.color = label.color,
                   label.size = label.size)
  p <- p+
    scale_fill_continuous(low = "white", high = color)+
    guides(fill = "none")
  
  slayer <- stat_density2d(aes_string(fill = "..density..^0.25"),
                           geom = "tile",
                           contour = FALSE,
                           n = nbin)
  
  p$layers <- c(slayer,p$layers)
  
  return(p)
}