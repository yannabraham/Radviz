#' Projects a Matrix or a Data Frame to a 2D space defined by Dimensional Anchors
#' 
#' do.radviz will return a projection of a multidimensional dataset onto a 2D space
#' defined by dimensional anchors that have been projected on the unit circle using
#' \code{\link{make.S}}
#' 
#' @param x a data.frame or matrix to be projected, with column names matching row names in springs
#' @param springs a matrix of 2D dimensional anchor coordinates, as returned by \code{\link{make.S}}
#' @param trans a transformation to be applied to the data before projection
#' @param label.color the color of springs for visualization
#' @param label.size the size of labels
#' @param type character string specifying the method used for obtaining the springs. 
#' 				Current methods are: Radviz, Freeviz and Graphviz. When not provided, \code{type} is 
#' 				derived from the other inputs
#' @param graph \code{igraph} object (only relevant for result obtained from \code{\link{do.optimGraphviz}} analysis) 
#' 
#' @details The function expects that at least some of the column names in df will be matched
#'            by row names in springs
#' 
#' @return an object of class radviz with the following slots:
#'			\itemize{
#' 				\item \code{proj}: a ggplot2 object with a single geom_text layer corresponding to springs.
#'            	the \code{data} slot of the ggplot2 corresponds to the input parameter \code{x}
#'            	with the following extra columns:
#'          		\itemize{
#'            			\item \code{rx} and \code{ry} the X and Y coordinates of the radviz projection of \code{x} over \code{springs}
#'            			\item \code{rvalid} an index of points corresponding to an invalid projection (any \code{rx} or \code{ry} is NA)
#' 					}
#' 				\item \code{type}: character string specifying the method used for obtaining the springs. 
#' 				\item \code{graphEdges}: when the input \code{graph} is provided (for a graphviz analysis), this slot will contain a 
#' 				dataframe with the graph edges
#' } 
#' 
#' @example examples/example-do.radviz.R
#' @examples
#' summary(rv)
#' @example examples/example-is.valid.R 
#' 
#' @aliases do.radviz do.radviz.default
#' 
#' @importFrom ggplot2 ggplot aes_string geom_text xlim ylim coord_equal
#' 
#' @aliases do.radviz do.radviz.default
#' @author Yann Abraham
#' @export
do.radviz <- function(x,
                      springs,
                      trans=do.L,
                      label.color='orangered4',
                      label.size=NA,
					  type=NULL,
					  graph=NULL
					  ) {
  ## check all springs are there
  if(!all(rownames(springs) %in% colnames(x))) {
    stop('The following springs are missing in the input:\n',
         paste(setdiff(rownames(springs),colnames(x)),sep='',collapse=', '))
  }
  
  ## if x is not a data frame then change it to one
  if(!is.data.frame(x)) {
    x <- as.data.frame(x)
  }
  
  ## extract the matrix
  mat <- as.matrix(x[,rownames(springs)])
  
  ## apply the transformation, if any
  if(!is.null(trans)) {
    mat <- apply(mat, 2, trans)
  }
  weights <- mat/matrix(rep(rowSums(mat),each=ncol(mat)),nrow=nrow(mat),byrow=T)
  rx <- colSums(t(weights)*springs[,1])
  ry <- colSums(t(weights)*springs[,2])
  rvd <- apply(cbind(rx,ry),1,function(x) any(is.na(x)))
  
  if(any(rvd)) {
    warning('at least 1 point could not be projected; check the `valid` slot for details')
  }
  
  # add the projection back to the data
  x[,'rx'] <- rx
  x[,'ry'] <- ry
  x[,'rvalid'] <- rvd
  
  # automatic check for type of method:
  if(is.null(type))	{
	  springLengths <-  diag(springs%*%t(springs))
	  if(any(springLengths < 0.99)){
		  type <- "Freeviz"
	  } else type <- "Radviz"
  }
  
  radviz <- list(proj=ggplot(data=x,
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
		
		if(!is.null(graph)){
			radviz$graphEdges <- igraph::as_data_frame(graph)
			radviz$type <- "Graphviz"
		}

  class(radviz) <- 'radviz'
  return(radviz)
}
