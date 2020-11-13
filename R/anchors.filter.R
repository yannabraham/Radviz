#' Filtering out anchors with low contributions to the projection
#' 
#' @param x a radviz object as produced by \code{\link{do.radviz}}
#' @param lim the minimum length of an anchor
#' 
#' @details When \code{anchors.filter} is a number and type is not Radviz, any springs whose length is lower than this number will be filtered out 
#' of the visualization. This has no effect on the projection itself.
#' 
#' @return a radviz object as produced by \code{\link{do.radviz}}
#' 
#' @example examples/example-do.radviz.R
#' @examples
#' plot(rv,anchors.only=FALSE)
#' new.S <- do.optimFreeviz(x = iris[,das], classes = iris$Species)
#' new.rv <- do.radviz(iris,new.S)
#' plot(new.rv,anchors.only=FALSE)
#' plot(anchors.filter(new.rv,0.2))
#' 
#' @author Yann Abraham
#' @importFrom dplyr `%>%` .data mutate filter
#' @export
anchors.filter <- function(x,
                           lim=0) {
  if(x$type=='Radviz') {
    warning('`anchors.filter` is not relevant for Radviz plots\n')
  } else {
    x$proj$layers[[1]]$data <- x$proj$layers[[1]]$data %>% 
      mutate(weight=(.data$X1^2+.data$X2^2)^0.5) %>% 
      filter(.data$weight>=lim)
  }
  return(x)
}
