#' Computes 2D density for contour plots of Radviz objects
#' 
#' Computes 2D density estimate of projected data for a Radivz object,
#' using the \code{\link[MASS]{kde2d}} function from the \pkg{MASS} package
#' 
#' @param x a radviz object as produced by do.radviz
#' @param n Number of grid points in each direction
#' @param method the method to use for KDE estimation
#' @param bandwidth the bandwidth to use for \link[KernSmooth]{bkde2D}
#' 
#' @details Computes a 2D density estimate of Radviz projected data and stores the results
#'            in a \code{density} slot of the Radviz object. Invalid points, if any, will be
#'            excluded.
#'            2 methods are implemented, \link[MASS]{kde2d} for \code{\link{contour.radviz}} and 
#'            \link[KernSmooth]{bkde2D} for \code{\link{smoothRadviz}}
#' 
#' @return the Radviz object with and extra slot \code{density} containing the 2D density
#' estimates for use with \code{\link{contour.radviz}} or \code{\link{smoothRadviz}}
#' 
#' @examples
#' data(iris)
#' das <- c('Sepal.Length','Sepal.Width','Petal.Length','Petal.Width')
#' S <- make.S(das)
#' rv <- do.radviz(iris,S)
#' rv <- do.density(rv)
#' rv <- do.density(rv,n=128,method='KernSmooth')
#' contour(rv,point.shape=1,point.color=c('red','green','blue')[as.integer(iris$Species)])
#' smoothRadviz(rv)
#' 
#' @seealso \link{contour.radviz} for plotting
#' @author Yann Abraham
#' @export
do.density <- function(x,n=50,method='MASS',bandwidth) {
  if(any(!x$valid)) {
    warning(sum(!x$valid)," point(s) could not be projected, which will not be used to compute density")
  }
  if(!method %in% c('MASS','KernSmooth')) {
    stop(method,' is not a valid method; see help(do.density) for valid options.')
  }
  if(method=='MASS') {
    x$contour <- MASS::kde2d(x$projected[x$valid,'x'],x$projected[x$valid,'y'],
                             n=n,
                             lims=c(c(-1.1,1.1),c(-1.1,1.1)))
  } else if(method=='KernSmooth') {
    if (missing(bandwidth)) {
      bandwidth <- diff(apply(x$projected[x$valid,], 2, quantile, probs = c(0.05, 0.95), names = FALSE))/25
      bandwidth[bandwidth == 0] <- 1
    }
    else {
      if (!is.numeric(bandwidth)) 
        stop("'bandwidth' must be numeric")
      if (any(bandwidth <= 0)) 
        stop("'bandwidth' must be positive")
    }
    x$smooth <- KernSmooth::bkde2D(x$projected[x$valid,],
                                   bandwidth = bandwidth,
                                   gridsize = c(n,n),
                                   range.x = list(c(-1.1,1.1),c(-1.1,1.1)))
  }
  return(x)
}
