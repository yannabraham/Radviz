#' Rescaling of projected data for plotting
#' 
#' @param x a radviz object as produced by \code{\link{do.radviz}}
#' @param fraction numeric value, indicating which fraction of the unit circle should be used for the rescaled plot
#' 
#' @details A different rescaling is used here for plotting the projected data as compared to \code{\link{do.radviz}}. 
#' Only feature-wise rescaling is applied to the original data (through \code{\link{do.L}}), in accordance with the rescaling used in 
#' \code{\link{do.optimFreeviz}} and \code{\link{do.optimGraphviz}}. The projected data is then rescaled based on amplitude, 
#' to cover a pre-specified fraction of the unit circle.
#' For \code{Freeviz} and \code{Graphviz} objects, the rescaling will issue a warning if some points extend beyond the some anchors: 
#' in that case only the direction of the anchor can be interpreted but not the magnitude represented by the anchor's position.
#' 
#' @return a radviz object as produced by \code{\link{do.radviz}}
#' 
#' @example examples/example-do.radviz.R
#' @examples
#' library(ggplot2)
#' plot(rv)+geom_point(aes(color=Species))
#' new.rv <- rescalePlot(rv)
#' plot(new.rv)+geom_point(aes(color=Species))
#' 
#' @author Nicolas Sauwen
#' @export
rescalePlot <- function(x, fraction=0.9){
	
	## compute which springs would end up below the new expansion for points and issue a warning for those
	if(x$type!='Radviz') {
	  springs <- x$proj$plot_env$springs
	  springs <- rowSums(springs^2)^0.5
	  springs <- sort(springs)
	  if(any(springs<fraction)) {
	    warning('After rescaling the following anchors will be below the maximum range of points:\n',
	            paste(names(springs)[springs<fraction],sep='',collapse=', '),'\n')
	  }
	}
	
  ## get the projected coordinates out
	rx <- unlist(x$proj$data[['rx']])
	ry <- unlist(x$proj$data[['ry']])
	rvd <- unlist(x$proj$data[['rvalid']])
	
	## Amplitude-based rescaling projected data
	ampl <- sqrt(rx^2+ry^2)
	ampl <- max(ampl[!rvd])
	rx <- rx/ampl*fraction
	ry <- ry/ampl*fraction
	
	# add the projection back to the data
	x$proj$data[,'rx'] <- rx
	x$proj$data[,'ry'] <- ry
	
	return(x)
}



