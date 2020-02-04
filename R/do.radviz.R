#' Projects a Matrix or a Data Frame to a 2D space defined by Dimensional Anchors
#' 
#' do.radviz will return a projection of a multidimensional dataset onto a 2D space
#' defined by dimensional anchors that have been projected on the unit circle using
#' \code{\link{make.S}}
#' 
#' @param x a data.frame or matrix to be projected, with column names matching row names in springs
#' @param springs A matrix of 2D dimensional anchor coordinates, as returned by \code{\link{make.S}}
#' 
#' @details The function expects that at least some of the column names in df will be matched
#'            by row names in springs
#' 
#' @return An object of class radviz with the following slots:
#'          \itemize{
#'            \item \code{data} the original data (\code{x})
#'            \item \code{springs} the original \code{springs}
#'            \item \code{projected} the projection of \code{x} on \code{springs},
#'                    a matrix of 2D coordinates for every line in df
#'            \item \code{valid} a logical vector 
#' 			  \item \code{type} character string, indicating method used for computing the projection (either "radviz", "freeviz", or "graphviz")
#' 		      \item \code{classes} vector with class labels of the observations (only used when \code{type) is "freeviz")  
#' 			  \item \code{graph} the original graph \code{igraph} object (only used when \code{type) is "graphviz")    
#'  		}
#' 
#' @examples
#' # the first example generates a simple Radviz object
#' data(iris)
#' das <- c('Sepal.Length','Sepal.Width','Petal.Length','Petal.Width')
#' S <- make.S(das)
#' rv <- do.radviz(iris,S)
#' summary(rv)
#' 
#' # in case a point cannot be projected, a warning will be raise
#' iris0 <- rbind(iris,c(rep(0,length(das)),NA))
#' rv0 <- do.radviz(iris0,S)
#' 
#' # to find out how many points could not be projected:
#' with(rv0,sum(!valid))
#' 
#' # to find which points where invalid in the data
#' with(rv0,which(!valid))
#' 
#' # to review the original data points
#' with(rv0,subset(data,!valid))
#' 
#' @aliases do.radviz do.radviz.default
#' @author Yann Abraham
#' @export
do.radviz <- function(x,springs, type = "radviz", classes = NULL, graph = NULL) {
  radviz <- list()
  radviz$data <- x
  radviz$springs <- springs
  mat <- as.matrix(x[,rownames(springs)])
  weights <- mat/matrix(rep(rowSums(mat),each=ncol(mat)),nrow=nrow(mat),byrow=T)
  rx <- colSums(t(weights)*springs[,1])
  ry <- colSums(t(weights)*springs[,2])
  proj <- data.frame(x=rx,y=ry)
  vald <- apply(proj,1,function(x) any(is.na(x)))
  if(any(vald)) {
    warning('at least 1 point could not be projected; check the `valid` slot for details')
  }
  row.names(proj) <- row.names(mat)
  radviz$projected <- proj
  radviz$valid <- unname(!vald)
  radviz$type <- type
  radviz$classes <- classes
  radviz$graph <- graph
  class(radviz) <- 'radviz'
  return(radviz)
}
