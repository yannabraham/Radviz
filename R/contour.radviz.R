#' 
#' Plots the Dimensional Anchors and density lines for projected data points in a 2D space.
#' 
#' @param x a radviz object as produced by do.radviz
#' @param main [Optional] a title to the graph, displayed on top 
#' @param color the variable in the Radviz projection used to color the contours
#' @param size The thickness of contour lines
#' @param label.color the color of springs for visualization
#' @param label.size the size of the anchors (see \href{https://ggplot2.tidyverse.org/articles/articles/faq-customising.html}{customizing ggplot2} for details on default value)
#' @param ...	further arguments to be passed to or from other methods (not implemented)
#' @param contour.color deprecated, see \code{\link[ggplot2]{geom_density2d}} instead
#' @param contour.size deprecated, see \code{\link[ggplot2]{geom_density2d}} instead
#' @param point.color deprecated, see \code{\link[ggplot2]{geom_density2d}} instead
#' @param point.shape deprecated, see \code{\link[ggplot2]{geom_density2d}} instead
#' @param point.size deprecated, see \code{\link[ggplot2]{geom_density2d}} instead
#' @param n deprecated, see \code{\link[ggplot2]{geom_density2d}} instead
#' @param drawlabels deprecated, see \code{\link[ggplot2]{geom_density2d}} instead
#' @param drawpoints deprecated, see \code{\link[ggplot2]{geom_density2d}} instead
#' @param add deprecated, see \code{\link[ggplot2]{geom_density2d}} instead
#' 
#' @return the internal ggplot2 object plus added layers, allowing for extra geoms to be added
#'  
#' @example examples/example-do.radviz.R
#' @examples 
#' contour(rv,color='Species')
#' 
#' @author Yann Abraham
#' @keywords multivariate hplot
#' @importFrom ggplot2 geom_density2d aes_string 
#' @export
contour.radviz <- function(x,...,
                           main=NULL,
                           color=NULL,
                           size=0.5,
                           label.color=NULL,
                           label.size=NULL,
                           contour.color,
                           contour.size,
                           point.color,
                           point.shape,
                           point.size,
                           n,
                           drawlabels,
                           drawpoints,
                           add) {
  ## check for deprecated arguments
  if(!missing(contour.color))
    warning('contour.color is a deprecated argument, use plot(x)+geom_density2d() and custom aes() to change plot.',call. = FALSE)
  if(!missing(contour.size))
    warning('contour.size is a deprecated argument, use plot(x)+geom_density2d() and custom aes() to change plot.',call. = FALSE)
  if(!missing(point.color))
    warning('point.color is a deprecated argument, use plot(x)+geom_density2d() and custom aes() to change plot.',call. = FALSE)
  if(!missing(point.shape))
    warning('point.shape is a deprecated argument, use plot(x)+geom_density2d() and custom aes() to change plot.',call. = FALSE)
  if(!missing(point.size))
    warning('point.size is a deprecated argument, use plot(x)+geom_density2d() and custom aes() to change plot.',call. = FALSE)
  if(!missing(n))
    warning('n is a deprecated argument, use plot(x)+geom_density2d() and custom aes() to change plot.',call. = FALSE)
  if(!missing(drawlabels))
    warning('drawlabels is a deprecated argument, use plot(x)+geom_density2d() and custom aes() to change plot.',call. = FALSE)
  if(!missing(drawpoints))
    warning('drawpoints is a deprecated argument, use plot(x)+geom_density2d() and custom aes() to change plot.',call. = FALSE)
  if(!missing(add))
    warning('add is a deprecated argument, use plot(x)+geom_density2d() and custom aes() to change plot.',call. = FALSE)
  ## plot
  p <- plot.radviz(x,
                   main = main,
                   label.color = label.color,
                   label.size = label.size)
  
  slayer <- geom_density2d(aes_string(color=color),
                           size=size)
  p$layers <- c(slayer,p$layers)
  return(p)
}
