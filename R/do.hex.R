#' Computes 2D hexagonal bins from a Radviz projection
#' 
#' Computes 2D density using hexagon binning of projected data for a Radivz object,
#' using the \code{\link[hexbin]{hexbin}} function from the \pkg{hexbin} package
#'
#' @param x a radviz object as produced by do.radviz
#' @param n Number of bins in the grid (see \link[hexbin]{hexbin} for details)
#' @param channels [optional] if channels is not \code{NULL},
#'          the function will compute color scales for the specified channels
#' @param colramp [optional] function accepting an integer n as an argument and returning n colors
#' @param ncols [optional] the number of levels used to cut each channels
#' @param use.quantile [optional] if channels is not \code{NULL} and use.quantile
#'          is \code{TRUE}, channels will be cut into \code{ncols+1} quantiles
#'          otherwise a fixed sequence from 0 to 1 in \code{ncols+1} steps will be used
#' @param fixed [optional] if channels is not \code{NULL} and fixed is \code{TRUE},
#'          channels will be cut into \code{length(fixed)+1} steps using the values in
#'          \code{fixed} as breaking points
#' 
#' @details The projected points will be binned into an hexagonal grid of size \code{n}.
#'            if channels is not \code{NULL}, for every channels the median intensity
#'            will be estimated over the complete grid and a color will be assigned
#'            from \code{colramp} using \code{ncols}, \code{fixed} and \code{use.quantile};
#'            by default, channels will be split using a fixed sequence from 0 to 1 over
#'            \code{ncols+1} levels unless \code{use.quantile} or \code{fixed} are set. 
#'            Invalid points (if any) will not be used in computing the bins or the colors.
#' 
#' @return the Radviz object with an extra slot \code{hex} containing the hexbin object;
#'          if \code{channels} is not \code{NULL} an extra \code{hexcols} will be present
#'          containing the color information for every channel
#'
#' @examples
#' data(iris)
#' das <- c('Sepal.Length','Sepal.Width','Petal.Length','Petal.Width')
#' S <- make.S(das)
#' rv <- do.radviz(iris,S)
#' rv <- do.hex(rv,channels='Sepal.Length',ncols=4,use.quantile=TRUE)
#' summary(rv$hex)
#' 
#' @author Yann Abraham
#' @author Dan Carr
#' @seealso \link{hexplot} for plotting, \link[hexbin]{hexbin} for original implementation
#' @export
do.hex <- function(x,n=30,channels=NULL,
                   colramp=function(n) colorRampPalette(c('yellow','grey','blue'))(n),
                   ncols=8,use.quantile=F,fixed=NULL) {
	cols <- colramp(ncols)
	do.colCut <- function(ch) {
		hexCh <- hexbin::hexTapply(
				x$hex,
				x$data[x$valid,ch],
				median
		)
		if(is.null(fixed)) {
			if(use.quantile) {
				bks <- quantile(hexCh,seq(0,1,length.out=ncols+1))
			} else {
				bks <- seq(0,1,length.out=ncols+1)
			}
			
		} else {
			bks <- c(0,fixed,1)
		}
		cut(
				hexCh,
				breaks=bks,
				labels=cols,
				include.lowest=T
		)
	}
	x$hex <- hexbin::hexbin(x$projected[x$valid,],xbins=n,IDs=TRUE)
	if(!is.null(fixed)) {
		ncols <- length(fixed+1)
	}
	if(!is.null(channels)) {
		if(!all(channels %in% dimnames(x$data)[[2]])) {
			x$missing <- channels[which(!channels %in% dimnames(x$data)[[2]])]
			channels <- channels[channels %in% dimnames(data)[[2]]]
			cat(length(missing),'channels are missing from input (see `missing` slot for details)')
		}
		x$hexcols <- lapply(channels,do.colCut)
		x$hexcols <- lapply(x$hexcols,as.character)
		names(x$hexcols) <- channels
	}
	return(x)
}