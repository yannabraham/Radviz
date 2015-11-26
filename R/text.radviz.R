#' Text annotations for for the Radviz Plots
#' 
#' Text draws the strings given in the vector labels at the coordinates given by the radviz projection
#' 
#' @param x a radviz object as produced by do.radviz
#' @param main [Optional] a title to the graph, displayed on top if add is \code{TRUE}
#' @param label.color [Optional] The color of the Dimensional Anchors if add is
#'          \code{TRUE} (defaults to orangered4)
#' @param label.size [Optional] numeric character expansion factor for Dimensional Anchor
#'          labels if add is \code{TRUE}; multiplied by \code{par("cex")} yields the final
#'          character size. NULL and NA are equivalent to 1.0
#' @param labels a character vector specifying the text to be written. An attempt is made
#'          to coerce other language objects (names and calls) to characters using
#'            \code{\link[grDevices]{as.graphicsAnnot}}. If labels is longer than x and y,
#'            the coordinates are recycled to the length of labels.
#' @param adj one or two values in [0, 1] which specify the x (and optionally y) adjustment
#'          of the labels. On most devices values outside that interval will also work.
#' @param pos a position specifier for the text. If specified this overrides any adj value given.
#'          Values of 1, 2, 3 and 4, respectively indicate positions below, to the left of, above
#'          and to the right of the specified coordinates.
#' @param offset when \code{pos} is specified, this value gives the offset of the label from the
#'          specified coordinate in fractions of a character width.
#' @param vfont \code{NULL} for the current font family, or a character vector of length 2 for
#'          Hershey vector fonts. The first element of the vector selects a typeface and the second
#'          element selects a style. Ignored if labels is an expression.
#' @param cex numeric character expansion factor; multiplied by \code{\link{par}("cex")} yields
#'          the final character size. NULL and NA are equivalent to 1.0.
#' @param col,font the color and (if vfont = NULL) font to be used, possibly vectors.
#'          These default to the values of the global \link{graphical parameters} in \link{par}().
#' @param add Logical: if add is \code{TRUE} then only the projected points are plotted
#' @param ...	further arguments to be passed to or from other methods
#' 
#' @examples
#' data(iris)
#' das <- c('Sepal.Length','Sepal.Width','Petal.Length','Petal.Width')
#' S <- make.S(das)
#' rv <- do.radviz(iris,S)
#' plot(rv,point.shape=1,point.color='grey')
#' med.iris <- split(iris,iris$Species)
#' med.iris <- lapply(med.iris,function(df) {
#'    spc <- unique(df$Species)
#'    df <-df[,names(df)!='Species']
#'    df <- apply(df,2,median)
#'    df <- data.frame(t(df))
#'    df$Species <- spc
#'    return(df)
#'  }
#' )
#' med.iris <- do.call('rbind',med.iris)
#' med.rv <- do.radviz(med.iris,S)
#' text(med.rv,labels=med.iris$Species,col=c('red','green','blue')[as.integer(med.iris$Species)])
#' 
#' @author Yann Abraham
#' @export
text.radviz <- function (x,..., main=NULL, label.color='orangered4', label.size=1,
                         labels = NULL, adj = NULL, pos = NULL, offset = 0.5,
                         vfont = NULL, cex = 1, col = NULL, font = NULL, add=TRUE) {
	
	labels <- as.graphicsAnnot(labels)
	if (!is.null(vfont)) 
		vfont <- c(typeface = pmatch(vfont[1L], Hershey$typeface),
		           fontindex = pmatch(vfont[2L], Hershey$fontindex))
	if (!add) {
		par(mar = c(0,0,1,0))
		plot(x$springs, type = "n", main = main, xlab = "",
		     ylab = "", xlim = c(-1.1, 1.1), ylim = c(-1.1, 1.1), 
		     frame.plot = F, axes = F)
		text(x$springs, labels = dimnames(x$springs)[[1]],
		     col = label.color,cex=label.size)
	}
	text(x$projected$x, x$projected$y, labels, adj, 
					pos, offset, vfont, cex, col, font)
}
