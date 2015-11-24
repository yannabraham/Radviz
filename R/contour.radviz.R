contour.radviz <-
function(x,main=NULL,label.color='orangered4',label.size=1,
		contour.color=par("fg"),contour.size=par('lwd'),
		point.color='lightgrey',point.shape='.',point.size=1,
		add=F,drawlabels=FALSE,drawpoints=FALSE,...) {
	par(mar = c(0,0,1,0))
	if (!add) {
		plot(x$springs, type = "n", main = main, xlab = "", 
				ylab = "", xlim = c(-1.1, 1.1), ylim = c(-1.1, 1.1), 
				frame.plot = F, axes = F)
		text(x$springs, labels = dimnames(x$springs)[[1]], 
				col = label.color,cex=label.size)
	}
	if(!'density' %in% names(x)) {
		x <- do.density(x) 
	}
	if(drawpoints) {
		points(x$projected, pch = point.shape, col = point.color, 
				cex = point.size)
	}
	contour(x$density,drawlabels=drawlabels,add=T,axes=F,frame.plot=F,col=contour.color,lwd=contour.size)
}
