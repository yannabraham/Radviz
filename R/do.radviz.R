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
#' 
#' @details The function expects that at least some of the column names in df will be matched
#'            by row names in springs
#' 
#' @return a ggplot2 object of class radviz with a single geom_text layer corresponding to springs.
#'            the \code{data} slot of the ggplot2 corresponds to the input parameter \code{x}
#'            with the following extra columns
#'          \itemize{
#'            \item \code{rx} and \code{ry} the X and Y coordinates of the radviz projection of \code{x} over \code{springs}
#'            \item \code{rvalid} an index of points corresponding to an invalid projection (any \code{rx} or \code{ry} is NA)
#'          }
#' 
#' @example examples/example-do.radviz.R
#' @examples
#' summary(rv)
#' @example examples/example-is.valid.R 
#' @aliases do.radviz do.radviz.default
#' 
#' @importFrom ggplot2 ggplot aes_string geom_text scale_x_continuous coord_equal theme_light theme element_blank
#' 
#' @author Yann Abraham
#' @export
do.radviz <- function(x,
                      springs,
                      trans=do.L,
                      label.color='orangered4',
                      label.size=NA) {
  ## check all springs are there
  if(!all(rownames(springs) %in% colnames(x))) {
    stop('The following springs are missing in the input:\n',
         paste(setdiff(rownames(springs),colnames(x)),sep='',collapse=', '))
  }
  ## if x is not a data frame then change it to one
  if(!class(x)=='data.frame') {
    x <- data.frame(x)
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
  # add the projection back to the data
  x[,'rx'] <- rx
  x[,'ry'] <- ry
  x[,'rvalid'] <- apply(x[,c('rx','ry')],1,function(x) any(is.na(x)))
  if(any(x[,'rvalid'])) {
    warning('at least 1 point could not be projected; check the `valid` slot for details')
  }
  radviz <- list(proj=ggplot(data=x,
                             aes_string(x='rx',y='ry'))+
                   geom_text(data = data.frame(springs,
                                               Channel=factor(rownames(springs),
                                                              levels=rownames(springs))),
                             aes_string(x='X1',y='X2',label='Channel'),
                             color=label.color,
                             size=label.size)+
                   scale_x_continuous(expand = c(0.1,0.05))+
                   coord_equal()+
                   theme_light()+
                   theme(axis.title.x=element_blank(),
                         axis.text.x=element_blank(),
                         axis.ticks.x=element_blank(),
                         axis.title.y=element_blank(),
                         axis.text.y=element_blank(),
                         axis.ticks.y=element_blank(),
                         panel.grid.major.x = element_blank(),
                         panel.grid.major.y = element_blank(),
                         panel.grid.minor.x = element_blank(),
                         panel.grid.minor.y = element_blank()))
  class(radviz) <- 'radviz'
  return(radviz)
}
