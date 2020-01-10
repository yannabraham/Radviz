#' Radviz Object Summary, head, print, dim and spings Methods
#' 
#' Provides a summary for Radviz objects
#' 
#' @param x an object of class Radviz, as returned by \code{\link{do.radviz}}
#' @param object an object of class Radviz, as returned by \code{\link{do.radviz}}
#' @param n the number of lines from each slots in the Radviz object to display (defaults to 6)
#' @param ...	further arguments to be passed to or from other methods (not implemented)
#' 
#' @example examples/example-do.radviz.R
#' @examples
#' 
#' summary(rv)
#' head(rv)
#' dim(rv)
#' print(rv)
#' 
#' @author Yann Abraham
#' @importFrom utils head
#' @export
summary.radviz <- function(object,...,n=6) {
  cat('A Radviz object with',nrow(object$proj$data),'objects and',nrow(object$proj$layers[[1]]$data),'dimensions\n')
  print(head(object$proj$data,n))
  if(any(object$proj$data$rvalid)) {
    cat(sum(object$proj$data$rvalid),'point(s) could not be projected\n')
  }
  das <- springs(object)
  if(length(das)>n) {
    das <- das[seq(1,n)]
    das <- c(das,'...')
  }
  cat('Dimensional Anchors are',das,'\n')
}

#' @rdname summary.radviz
#' @export
head.radviz <- function(x,n=6,...) {
  print(head(x$proj$data,n=n))
}

#' @rdname summary.radviz
#' @export
dim.radviz <- function(x) {
  dim(x$proj$data)
}

#' @rdname summary.radviz
#' @export
print.radviz <- function(x,...) {
  print(x$proj)
  return(invisible(x$proj$data))
}

#' @rdname summary.radviz
#' @export
springs <- function(x) {
  return(levels(x$proj$layers[[1]]$data$Channel))
}