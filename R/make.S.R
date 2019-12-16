#' Define Dimensional Anchors around the Unit Circle
#' 
#' make.S will return [x,y] coordinates for n dimensional anchors equally spaced around the unit circle
#' 
#' @param x a vector of dimensional anchors, or a list of dimensional anchors for Class Discrimination Layout, 
#' or the number of anchors to put on the circle
#' 
#' @details If x is a vector or a list, values will be used to set the row names of the matrix.
#' 
#' @return A matrix with 2 columns (x and y coordinates of dimensional anchors) and 1 line
#'          per dimensional anchor (so called springs). If x is a vector, the row names of
#'          the matrix will be set to the syntactically correct version of values in the vector
#'          (through a call to \code{\link[base]{make.names}}). Please note that some functions
#'          expect to match column names of data to row names of the spring matrix.
#' 
#' @examples
#' data(iris)
#' das <- c('Sepal.Length','Sepal.Width','Petal.Length','Petal.Width')
#' make.S(length(das)) # without row names
#' make.S(das) # with row names
#' make.S(list(c('Sepal.Length','Sepal.Width'),c('Petal.Length','Petal.Width')))
#' 
#' @author Yann Abraham
#' @export
make.S <- function(x) {
  if(is.list(x)) {
    n.class <- length(x)
    r <- 1
    theta.class <- -2*pi/n.class
    S <- lapply(seq(0,n.class-1),function(j) {
      n.par <- length(x[[j+1]])
      theta <- theta.class/(n.par+1)
      sub.S <- lapply(seq(1, n.par), function(i) {
        xi <- round(r * cos(pi/2 + j*theta.class + i * theta), 2)
        yi <- round(r * sin(pi/2 + j*theta.class + i * theta), 2)
        return(c(xi, yi))
      })
      sub.S <- do.call('rbind',sub.S)
      return(sub.S)
    })
    S <- do.call("rbind", S)
    S <- as.matrix(S)
    rownames(S) <- make.names(unlist(x))
  } else {
    if(is.numeric(x)) {
      n.par <- x
    } else {
      n.par <- length(x)
    }
    r <- 1
    theta <- -2*pi/n.par
    S <- lapply(seq(0,n.par-1), function(i) {
      xi <- round(r*cos(pi/2+i*theta),2)
      yi <- round(r*sin(pi/2+i*theta),2)
      return(c(xi,yi))
    })
    S <- do.call('rbind',S)
    S <- as.matrix(S)
    if(!is.numeric(x)) {
      rownames(S) <- make.names(x)
    }
  }
  return(S)
}
