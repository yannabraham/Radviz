bubbleRadviz <-
function(x, main = NULL, label.color = "orangered4", label.size=1, bubble.color = "grey", bubble.fg='white',
		bubble.size = 1, scale=0.5, decreasing=NA, add=FALSE) {
	if(length(bubble.color)==1) {
		bubble.color <- rep(bubble.color,nrow(x$projected))
	}
	if(length(bubble.size)==1) {
		bubble.size <- rep(bubble.size,nrow(x$projected))
	}
	par(mar = c(0,0,1,0))
	if(!add) {
		plot(x$springs, type = "n",xlab = "", 
				ylab = "", xlim = c(-1.1, 1.1), ylim = c(-1.1, 1.1), 
				frame.plot = F, axes = F, main = main
		)
		text(x$springs,
				labels = dimnames(x$springs)[[1]],
				col=label.color,
				cex=label.size
		)
	}
	if(!is.na(decreasing)) {
		if(is.logical(decreasing)) {
			x$projected <- x$projected[order(bubble.size,decreasing=decreasing),]
			bubble.color <- bubble.color[order(bubble.size,decreasing=decreasing)]
			bubble.size <- sort(bubble.size,decreasing=decreasing)
		} else {
			if(length(decreasing)==nrow(x$projected)) {
				x$projected <- x$projected[decreasing,]
				bubble.color <- bubble.color[decreasing]
				bubble.size <- bubble.size[decreasing]
			}
		}
	}
	symbols(x$projected,circles=bubble.size,bg=bubble.color,fg=bubble.fg,inches=scale,add=!add)
	return(invisible(NULL))
}