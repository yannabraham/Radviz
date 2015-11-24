do.density <-
function(x,n=50) {
	x$density <- MASS::kde2d(x$projected[,'x'],x$projected[,'y'],
			n=n,
			lims=c(c(-1.1,1.1),c(-1.1,1.1)))
	return(x)
}