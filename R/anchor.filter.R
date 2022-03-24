#' Filtering out anchors with low contributions to the projection
#' 
#' @param x a radviz object as produced by \code{\link{do.radviz}}
#' @param lim the minimum length of an anchor
#' 
#' @details When \code{anchor.filter} is a number and type is not Radviz, 
#' any springs whose length is lower than this number will be filtered out 
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
#' plot(anchor.filter(new.rv,0.2))
#' 
#' @author Yann Abraham
#' @export
anchor.filter <- function(x,
                           lim=0) {
  if(x$type=='Radviz') {
    warning('`anchor.filter` is not relevant for Radviz plots\n')
  } else {
    weight <- rowSums(x$springs^2)^0.5
    x$springs <- x$springs[weight>=lim,]
  }
  return(x)
}
