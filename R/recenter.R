#' Rotate Dimensional Anchors around the Unit Circle
#' 
#' recenter will rotate the order of the dimensional anchors around the circle, to put a channel
#' of reference to the top of the display.
#' 
#' @param springs a spring object as created by \code{\link{make.S}}
#' @param newc a string specifying which dimensional anchor should be placed on top of the unit circle
#' 
#' @return a spring object with rotated labels
#' 
#' @examples
#' data(iris)
#' das <- c('Sepal.Length','Sepal.Width','Petal.Length','Petal.Width')
#' iris.S <- make.S(das)
#' iris.S
#' recenter(iris.S,'Petal.Length')
#' 
#' @author Yann Abraham
#' @export
recenter <- function(springs,newc) {
  if(any(rownames(springs)==newc)) {
    posc <- which(rownames(springs)==newc)
    rownames(springs) <- c(rownames(springs)[posc:nrow(springs)],
                           rownames(springs)[1:(posc-1)])
    return(springs)
  } else {
    stop(cat(newc,'was not found in springs\n'))
  }
}
