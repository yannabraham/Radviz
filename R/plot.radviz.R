plot.radviz <-
function(x,main=NULL,label.color='orangered4',label.size=1,point.color='black',point.shape='.',point.size=1,add=F,...) {
	par(mar = c(0,0,1,0))
	if (!add) {
		plot(x$springs, type = "n", main = main, xlab = "", 
				ylab = "", xlim = c(-1.1, 1.1), ylim = c(-1.1, 1.1), 
				frame.plot = F, axes = F)
		text(x$springs, labels = dimnames(x$springs)[[1]], 
				col = label.color,cex=label.size)
	}
	points(x$projected, pch = point.shape, col = point.color, 
			cex = point.size)
}
