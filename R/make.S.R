make.S <-
function(x) {
	if(is.numeric(x)) {
		n.par <- x
	} else {
		n.par <- length(x)
	}
	r <- 1
	c <- c(0,0)
	theta <- -2*pi/n.par
	S <- lapply(seq(0,n.par-1), function(i) {
				xi <- round(r*cos(pi/2+i*theta),2)
				yi <- round(r*sin(pi/2+i*theta),2)
				return(c(xi,yi))
			}
	)
	S <- do.call('rbind',S)
	S <- as.matrix(S)
	if(!is.numeric(x)) {
		rownames(S) <- make.names(x)
	}
	return(S)
}
