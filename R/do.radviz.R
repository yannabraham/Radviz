#' Projects a Matrix or a Data Frame to a 2D space defined by Dimensional Anchors
#' 
#' do.radviz will return a projection of a multidimensional dataset onto a 2D space
#' defined by dimensional anchors that have been projected on the unit circle using
#' \code{\link{make.S}}
#' 
#' @param x a data.frame or matrix to be projected, with column names matching row names in springs
#' @param springs a matrix of 2D dimensional anchor coordinates, as returned by \code{\link{make.S}}
#' @param scaling a scaling factor applied to data before the projection.
#' @param trans a transformation to be applied to the data before projection
#' @param type character string specifying the method used for obtaining the springs. 
#' 				Current methods are: Radviz, Freeviz and Graphviz. When not provided, \code{type} is 
#' 				derived from the other inputs
#' @param graph \code{igraph} object (only relevant for result obtained from \code{\link{do.optimGraphviz}} analysis) 
#' @param label.color deprecated, use \code{\link{plot.radviz}} instead
#' @param label.size deprecated, use \code{\link{plot.radviz}} instead
#' 
#' @details The function expects that at least some of the column names in x 
#' will be matched by all row names in springs.
#' The scaling factor can be used to increase the distance between points, 
#' making it useful in situations where all points are pulled together either 
#' because of similar values or large number of channels.
#' The scaling is applied **after** the transformation by \code{trans}.
#' The scaling idea is taken from [Artur & Minghim 2019](https://doi.org/10.1016/j.cag.2019.08.015).
#' 
#' @return an object of class radviz with the following slots:
#'			\itemize{
#' 				\item \code{proj}: a ggplot2 object with a single geom_text layer corresponding to springs.
#'            	the \code{data} slot of the ggplot2 corresponds to the input parameter \code{x}
#'            	with the following extra columns:
#'          		\itemize{
#'            			\item \code{rx} and \code{ry} the X and Y coordinates of the radviz projection of \code{x} over \code{springs}
#'            			\item \code{rvalid} an index of points corresponding to an invalid projection (any \code{rx} or \code{ry} is NA)
#' 				}
#' 				\item \code{springs}: the matrix containing the spring coordinates.
#' 				\item \code{type}: character string specifying the method used for obtaining the springs.
#' 				\item \code{trans}: the function used to transform the data.
#'        \item \code{graphEdges}: when the input \code{graph} is provided (for a Graphviz analysis),
#'              this slot will contain a dataframe with the graph edges
#' } 
#' 
#' @example examples/example-do.radviz.R
#' @examples
#' summary(rv)
#' @example examples/example-is.valid.R 
#' 
#' @aliases do.radviz do.radviz.default
#' 
#' @importFrom ggplot2 ggplot aes_string geom_text xlim ylim coord_equal GeomLabel
#' 
#' @aliases do.radviz do.radviz.default
#' @author Yann Abraham
#' @export
do.radviz <- function(x,
                      springs,
                      scaling=1,
                      trans=do.L,
                      label.color='orangered4',
                      label.size=NA,
                      type=NULL,
                      graph=NULL) {
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
  weights <- mat/matrix(rep(rowSums(mat^scaling),
                            each=ncol(mat)),
                        nrow=nrow(mat),
                        byrow=T)
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
  
  # find axis range
  lims <- range(springs)*1.1
  
  radviz <- list(proj=ggplot(data=x,
                             aes_string(x='rx',y='ry'))+
                   coord_equal()+
                   xlim(lims)+
                   ylim(lims)+
                   theme_radviz(),
                 springs = springs,
                 type=type,
                 trans=trans)
  
  if(!is.null(graph)){
    radviz$graphEdges <- igraph::as_data_frame(graph)
    radviz$type <- "Graphviz"
  }
  
  class(radviz) <- 'radviz'
  return(radviz)
}
