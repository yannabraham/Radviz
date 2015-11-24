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