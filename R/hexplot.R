hexplot <-
function(x,main=NULL,label.color='orangered4',label.size=1,mincnt=0,color=NULL,style='constant.col') {
	if(!'hex' %in% names(x)) {
		stop('Hexbin have not been run on radviz projection')
	}
	par(mar = c(0, 0, 1, 0))
	plot(x$springs, type = "n",xlab = "", 
			ylab = "", xlim = c(-1.1, 1.1), ylim = c(-1.1, 1.1), 
			frame.plot = F, axes = F)
	vps <- gridBase::baseViewports()
	grid::pushViewport(vps$inner)
	grid::pushViewport(vps$figure)
	grid::pushViewport(vps$plot)
	if(!is.null(color)) {
		if(color %in% names(x$hexcols)) {
			hexbin::grid.hexagons(x$hex,
					mincnt=mincnt,
					style=style,
					pen = x$hexcols[[color]][x$hex@count>=mincnt]
			)
		}
	} else {
		hexbin::grid.hexagons(x$hex,mincnt=mincnt)
	}
	text(x$springs, labels = dimnames(x$springs)[[1]],col=label.color,cex=label.size)
	if(!is.null(main)) {
		if(!is.null(color)) {
			title(main=paste(main,' (',color,')',sep=''	) )
		} else {
			title(main=main)
		}
	} else {
		title(main=color)
	}
	grid::popViewport()
	grid::popViewport()
	grid::popViewport()
}