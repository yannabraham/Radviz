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
#'          }
#' 
#' @examples
#' data(iris)
#' das <- c('Sepal.Length','Sepal.Width','Petal.Length','Petal.Width')
#' S <- make.S(das)
#' rv <- do.radviz(iris,S)
#' summary(rv)
#' 
#' @aliases do.radviz do.radviz.default
#' @author Yann Abraham
#' @export
do.radviz <- function(x,springs) {
  radviz <- list()
  radviz$data <- x
  radviz$springs <- springs
  mat <- as.matrix(x[,rownames(springs)])
  weights <- mat/matrix(rep(rowSums(mat),each=ncol(mat)),nrow=nrow(mat),byrow=T)
  rx <- colSums(t(weights)*springs[,1])
  ry <- colSums(t(weights)*springs[,2])
  proj <- data.frame(x=rx,y=ry)
  row.names(proj) <- row.names(mat)
  radviz$projected <- proj
  class(radviz) <- 'radviz'
  return(radviz)
}
