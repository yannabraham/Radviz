#' A Plotting Function for the Radviz Object
#' 
#' Plots the Dimensional Anchors and projected data points in a 2D space.
#' 
#' @param x a radviz object as produced by \code{\link{do.radviz}}
#' @param main [Optional] a title to the graph, displayed on top
#' @param anchors.only by default only plot the anchors so that other methods can easily be chained
#' @param anchors.filter filter out anchors with low contributions to the projection
#' @param label.color the color of springs for visualization
#' @param label.size the size of labels
#' @param ...	further arguments to be passed to or from other methods (not implemented)
#' @param point.color deprecated, use \code{\link{geom_point}} instead
#' @param point.shape deprecated, use \code{\link{geom_point}} instead
#' @param point.size deprecated, use \code{\link{geom_point}} instead
#' @param add deprecated, use \code{\link{geom_point}} instead
#' 
#' @details by default the plot function only shows the anchors. Extra geoms are required to display the data.
#' When \code{anchors.filter} is a number and type is not Radviz, any springs whose length is lower than this number will be filtered out 
#' of the visualization. This has no effect on the projection itself. 
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
                        anchors.filter=NULL,
                        label.color=NULL,
                        label.size=NULL,
                        point.color,
                        point.shape,
                        point.size,
                        add,
                        ...) {
  ## check for deprecated arguments
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
  if(is.null(label.size)) {
    label.size <- NA
  }
  if(is.null(label.color)) {
    label.color <- 'orangered4'
  }
  if(!is.numeric(label.size)){
    label.size <- as.numeric(label.size)
  }
  if(!is.null(anchors.filter) & x$type!='Radviz') {
    r <- rowSums(x$proj$plot_env$springs^2)^0.5
    df <- p$layers[[1]]$data %>% 
      subset(r>=anchors.filter)
  } else if(!is.null(anchors.filter) & x$type=='Radviz') {
    warning('`anchors.filter` is not relevant for Radviz plots\n')
    df <- p$layers[[1]]$data
  } else {
    df <- p$layers[[1]]$data
  }
  if(!is.null(label.color) | !is.null(label.size) | !is.null(anchors.filter)) {
    p$layers[[1]] <- geom_text(data = df,
                               aes_string(x='X1',y='X2',label='Channel'),
                               color=label.color,
                               size=label.size)
  }
  
  return(p)
}
