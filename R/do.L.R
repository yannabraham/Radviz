#' Perform L-Normalization on a Vector
#' 
#' Standardizes all values in a vector to the unit vector ([0,1]) using local min and max
#' 
#' @param v a vector of values
#' @param fun a function that will return the minimum and maximum values to use to scale v;
#'          defaults to \code{\link[base]{range}}
#' @param na.rm Logical: should NA be removed? defaults to \code{TRUE}
#' 
#' @details This is an alternative to performing a L normalization over the full matrix. 
#' 
#' @return A vector of values of the same lenght as x, scaled to the unit vector.
#' 
#' @examples
#' data(iris)
#' mat <- iris[,c('Sepal.Length','Sepal.Width','Petal.Length','Petal.Width')]
#' scaled <- apply(mat,2,do.L)
#' summary(scaled) # all values are between [0,1]
#' 
#' @author Yann Abraham
#' @export
do.L <- function(v,fun=range,na.rm=T) {
	# if requested remove the na values
  if(na.rm) {
		clean_v <- v[!is.na(v)]
	} else {
	  clean_v <- v
	}
  # find the min and the max
  range_v <- match.fun(fun)(clean_v)
	min_v <- min(range_v)
	max_v <- max(range_v)
	# scale v
	scaled_v <- (v-min_v)/(max_v-min_v)
	# remove values above and below 1 and 0
	scaled_v[scaled_v<0] <- 0
	scaled_v[scaled_v>1] <- 1
	return(scaled_v)
}
