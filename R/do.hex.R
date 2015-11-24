do.hex <-
function(x, n=30,channels=NULL,colramp=function(n) colorspace::diverge_hcl(n),ncols=8,use.quantile=F,fixed=NULL) {
	cols <- colramp(ncols)
	do.colCut <- function(ch) {
		hexCh <- hexbin::hexTapply(
				x$hex,
				x$data[,ch],
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
	x$hex <- hexbin::hexbin(x$projected,xbins=n,IDs=TRUE)
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