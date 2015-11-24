text.radviz <- function (x, main=NULL, label.color='orangered4',label.size=1, labels = NULL, adj = NULL, pos = NULL, 
		offset = 0.5, vfont = NULL, cex = 1, col = NULL, font = NULL,
		add=TRUE,...) {
	
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
					pos, offset, vfont, cex, col, font, ...)
}
