#' Rescaling of projected data for plotting
#' 
#' @param x a radviz object as produced by \code{\link{do.radviz}}
#' @param fraction numeric value, indicating which fraction of the unit circle should be used for the rescaled plot
#' 
#' @details A different rescaling is used here for plotting the projected data as compared to \code{\link{do.radviz}}. 
#' Only feature-wise rescaling is applied to the original data (through \code{\link{do.L}}), in accordance with the rescaling used in 
#' \code{\link{do.optimFreeviz}} and \code{\link{do.optimGraphviz}}. The projected data is then rescaled based on amplitude, to cover a pre-specified fraction of the unit circle
#' @return a radviz object as produced by \code{\link{do.radviz}}
#' 
#' @example examples/example-do.radviz.R
#' @examples
#' plot(rv)+geom_point(aes(color=Species))
#' new.rv <- rescalePlot(rv)
#' plot(new.rv)+geom_point(aes(color=Species))
#' 
#' @author Nicolas Sauwen
#' @export
rescalePlot <- function(x, fraction=0.9){
	
	# Extract relevant data back from Radviz object
	springs <- x$proj$plot_env$springs
	data <- x$proj$data
	data <- data[,-c((ncol(data)-2):ncol(data))]
	
	## extract the matrix
	mat <- as.matrix(data[,rownames(springs)])
	mat <- apply(mat, 2, do.L)
	
	## Compute projection
	rx <- mat%*%springs[,1]
	ry <- mat%*%springs[,2]
	rvd <- apply(cbind(rx,ry),1,function(x) any(is.na(x)))
	
	## Amplitude-based rescaling projected data
	ampl <- sqrt(rx^2+ry^2)
	rx <- rx/max(ampl)*fraction
	ry <- ry/max(ampl)*fraction
	
	# add the projection back to the data
	data[,'rx'] <- rx
	data[,'ry'] <- ry
	data[,'rvalid'] <- rvd
	
	type <- x$type
	label.color <- x$proj$plot_env$label.color
	label.size <- x$proj$plot_env$label.size
	
	radviz <- list(proj=ggplot(data=data,
							aes_string(x='rx',y='ry'))+
					geom_text(data = data.frame(springs,
									Channel=factor(rownames(springs),
											levels=rownames(springs))),
							aes_string(x='X1',y='X2',label='Channel'),
							color=label.color,
							size=label.size)+
					coord_equal()+
					xlim(c(-1.1,1.1))+
					ylim(c(-1,1))+
					theme_radviz(),
			type=type)
	
	if(!is.null(x$graphEdges)){
		radviz$graphEdges <- x$graphEdges
	}
	
	class(radviz) <- 'radviz'
	return(radviz)
}



