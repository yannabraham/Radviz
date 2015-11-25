#' A smoothScatter function for Radviz objects
#' 
#' Plots the Dimensional Anchors and a smoothed color density
#' representation of projected data points in a 2D space.
#' 
#' @param x a radviz object as produced by \code{\link{do.radviz}}
#' @param main [Optional] a title to the graph, displayed on top
#' @param label.color The color of the Dimensional Anchors (defaults to orangered4)
#' @param label.size numeric character expansion factor for Dimensional Anchor labels;
#'          multiplied by \code{par("cex")} yields the final character size.
#'          NULL and NA are equivalent to 1.0
#' @param smooth.color \code{function} accepting an integer n as an argument and returning
#'          n colors (see \link[graphics]{smoothScatter} for details)
#' @param transformation \code{function} mapping the density scale to the color scale
#' @param nbin numeric vector of length one (for both directions) or two (for x and y separately)
#'          specifying the number of equally spaced grid points for the density estimation;
#'          directly used as gridsize in \link[KernSmooth]{bkde2D} (see \link[graphics]{smoothScatter}
#'          for details)
#' @param nrpoints number of points to be superimposed on the density image
#'          (see \link[graphics]{smoothScatter} for details)
#' @param bandwidth numeric vector (length 1 or 2) of smoothing bandwidth(s).
#'          If missing, a more or less useful default is used. bandwidth is subsequently
#'          passed to function \link[KernSmooth]{bkde2D} (see \link[graphics]{smoothScatter} for details)
#' 
#' @details The add allows plotting of additional data such as cluster centers onto an existing plot.
#' 
#' @examples
#' data(iris)
#' das <- c('Sepal.Length','Sepal.Width','Petal.Length','Petal.Width')
#' S <- make.S(das)
#' rv <- do.radviz(iris,S)
#' smoothRadviz(rv)
#' 
#' @seealso \code{\link[graphics]{smoothScatter}} for original implementation
#' @author Yann Abraham
#' @author Florian Hahne
#' @export
#' 
smoothRadviz <- function (x, main = NULL, label.color = "orangered4", label.size = 1, 
				smooth.color = colorRampPalette(c("white", blues9)),
				transformation = function(x) x^0.25, nbin=128, nrpoints = 100, bandwidth) 
{
	# taken from the original smoothScatter function
	if (!is.numeric(nrpoints) | (nrpoints < 0) | (length(nrpoints) != 1)) 
		stop("'nrpoints' should be numeric scalar with value >= 0.")
	if (length(nbin) == 1) 
		nbin <- c(nbin, nbin)
	if (!is.numeric(nbin) || length(nbin) != 2) 
		stop("'nbin' must be numeric of length 1 or 2")
	if (missing(bandwidth)) {
		bandwidth <- diff(apply(x$projected, 2, quantile, probs = c(0.05, 0.95), na.rm = TRUE, names = FALSE))/25
		bandwidth[bandwidth == 0] <- 1
	}
	else {
		if (!is.numeric(bandwidth)) 
			stop("'bandwidth' must be numeric")
		if (any(bandwidth <= 0)) 
			stop("'bandwidth' must be positive")
	}
	map <- KernSmooth::bkde2D(x$projected,
			bandwidth = bandwidth,
			gridsize = nbin,
			range.x = list(c(-1.1,1.1),c(-1.1,1.1))
	)
	xm <- map$x1
	ym <- map$x2
	dens <- map$fhat
	dens[] <- transformation(dens)
	# plot
	par(mar = c(0, 0, 1, 0), bty = "n")
	image(xm, ym, z = dens, col = smooth.color(256), xlim = c(-1.1, 1.1), ylim = c(-1.1, 1.1),axes=F)
	text(x$springs, labels = dimnames(x$springs)[[1]], cex = label.size, 
			col = label.color)
	title(main)
	return(invisible(NULL))
}